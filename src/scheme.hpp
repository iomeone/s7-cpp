#ifndef SCHEME_HPP
#define SCHEME_HPP

#include <s7.h>

struct Scheme {

    Scheme(): scheme(nullptr) {}
    Scheme(s7_scheme* scheme_): scheme(scheme_) {}

    void load(const char* file);

    template<typename... Vals>
    void define_constant(tuple<const char*, Vals, const char*>&& ... vals);

    template<auto P>
    void define_function(const char* name, const char* doc = "");

    template<typename Ret, typename... Args>
    decltype(auto) apply(const char* func, Args... args);

    template<typename T>
    optional<T> from(s7_pointer);

    template<typename T>
    s7_pointer into(T);

    template<typename CAR>
    s7_pointer cons(CAR car, s7_pointer cdr);

    template<typename CAR, typename... Rest>
    s7_pointer list(CAR car, Rest... rest);

    template<typename CAR>
    s7_pointer list(CAR car);

    template<typename T>
    s7_pointer list(const vector<T>& vec);

    template<typename... TY>
    tuple<optional<TY>...> to_tuple(s7_pointer list);

  private:

    template<typename... TY, size_t... I>
    auto to_tuple_impl(s7_pointer cons, std::index_sequence<I...>) {
        return std::make_tuple(from<TY>(s7_list_ref(scheme, cons, I))...);
    }

    s7_scheme* scheme;
};

template<auto, typename>
struct scheme_function_generator_helper {};

template<auto F, typename Ret, typename... Args>
struct scheme_function_generator_helper<F, Ret(Args...)> {
    static s7_pointer call(s7_scheme*, s7_pointer);
    constexpr static size_t arg_count = sizeof...(Args);
};

template<auto F, typename Ret, typename... Args>
struct scheme_function_generator_helper<F, Ret(Scheme*, Args...)> {
    static s7_pointer call(s7_scheme*, s7_pointer);
    constexpr static size_t arg_count = sizeof...(Args);
};

template<auto F, typename Ret, typename... Args>
inline s7_pointer scheme_function_generator_helper<F, Ret(Args...)>::call(
    s7_scheme* scheme_,
    s7_pointer args) {
    Scheme scheme(scheme_);
    auto given =
        map_tuple([](auto v) { return *v; }, scheme.to_tuple<Args...>(args));
    if constexpr (std::is_void_v<Ret>) {
        std::apply(F, given);
        return scheme.into(false);
    } else {
        return scheme.into(std::apply(F, given));
    }
}

template<auto F, typename Ret, typename... Args>
inline s7_pointer scheme_function_generator_helper<F,
                                                   Ret(Scheme*, Args...)>::call(
    s7_scheme* scheme_,
    s7_pointer args) {
    Scheme scheme(scheme_);
    auto given = std::tuple_cat(
        std::make_tuple(&scheme),
        map_tuple([](auto v) { return *v; }, scheme.to_tuple<Args...>(args)));
    if constexpr (std::is_void_v<Ret>) {
        std::apply(F, given);
        return scheme.into(false);
    } else {
        return scheme.into(std::apply(F, given));
    }
}


template<auto F, typename Ret, typename... Args, typename C>
struct scheme_function_generator_helper<F, Ret (C::*)(Args...)> {
    static s7_pointer call(s7_scheme*, s7_pointer);
    static Ret inner(C* this_, Args... args) {
        return (this_->*F)(args...);
    }
    constexpr static size_t arg_count = sizeof...(Args) + 1;
};

template<auto F, typename Ret, typename... Args, typename C>
struct scheme_function_generator_helper<F, Ret (C::*)(Scheme*, Args...)> {
    static s7_pointer call(s7_scheme*, s7_pointer);
    static Ret inner(Scheme* scheme, C* this_, Args... args) {
        return (this_->*F)(scheme,
                           args...);
    }
    constexpr static size_t arg_count = sizeof...(Args) + 1;
};

template<auto F, typename Ret, typename... Args, typename C>
inline s7_pointer scheme_function_generator_helper<F, Ret (C::*)(Scheme*,
                                                                 Args...)>::call(
    s7_scheme* scheme_,
    s7_pointer args) {
    return scheme_function_generator_helper<scheme_function_generator_helper<F,
                                                                             Ret (C::*)(
                                                                                 Scheme*,
                                                                                 Args...)>::inner,
                                            Ret(Scheme*,
                                                C*,
                                                Args...)>
           ::call(
        scheme_,
        args);
}

template<auto F, typename Ret, typename... Args, typename C>
inline s7_pointer scheme_function_generator_helper<F,
                                                   Ret (C::*)(Args...)>::call(
    s7_scheme* scheme_,
    s7_pointer args) {
    return scheme_function_generator_helper<scheme_function_generator_helper<F,
                                                                             Ret (C::*)(
                                                                                 Args...)>::inner,
                                            Ret(C*, Args...)>::call(scheme_,
                                                                    args);
}

template<auto F, typename T = decltype(F)>
struct scheme_function_generator: scheme_function_generator_helper<F,
                                                                   std::remove_pointer_t<std::remove_cv_t<T>>>
{};


/* Inline definitions */

template<auto P>
inline void Scheme::define_function(const char* name, const char* doc) {
    s7_define_function(scheme,
                       name,
                       scheme_function_generator<P>::call,
                       scheme_function_generator<P>::arg_count,
                       0,
                       false,
                       doc);
}

template<typename... Vals>
inline void Scheme::define_constant(tuple<const char*, Vals,
                                          const char*>&& ... vals) {
    auto conv = [this](auto&& val) { return into(std::get<1>(val)); };
    std::array<const char*, sizeof...(Vals)> names = {std::get<0>(vals)...};
    std::array<s7_pointer, sizeof...(Vals)> stuff = {conv(vals)...};
    std::array<const char*, sizeof...(Vals)> docs = {std::get<2>(vals)...};
    for (size_t i = 0; i < sizeof...(Vals); ++i) {
        s7_define_constant_with_documentation(scheme,
                                              names[i],
                                              stuff[i],
                                              docs[i]);
    }
}

template<typename Ret, typename... Args>
inline decltype(auto) Scheme::apply(const char* func, Args... args) {
    if constexpr (std::is_void_v<Ret>) {
        s7_call(scheme,
                s7_name_to_value(scheme, func),
                list(args...));
    } else {
        return from<Ret>(s7_call(scheme,
                                 s7_name_to_value(scheme, func),
                                 list(args...)));
    }
}

template<typename CAR>
inline s7_pointer Scheme::cons(CAR car, s7_pointer cdr) {
    return s7_cons(scheme, into(car), cdr);
}

template<typename CAR, typename... Rest>
inline s7_pointer Scheme::list(CAR car, Rest... rest) {
    return cons(car, list(rest...));
}

template<typename CAR>
inline s7_pointer Scheme::list(CAR car) {
    return cons(car, s7_nil(scheme));
}

template<typename T>
inline optional<T> Scheme::from(s7_pointer val) {
    if constexpr (std::is_same_v<s7_pointer, T>) {
        return optional<s7_pointer>(val);
    } else if constexpr (std::is_same_v<bool, T>) {
        return (s7_is_boolean(val))
               ? optional<bool>(s7_boolean(scheme, val))
               : optional<bool>();
    } else if constexpr (std::is_integral_v<T>) {
        return (s7_is_integer(val))
               ? optional<T>(static_cast<T>(s7_integer(val)))
               : optional<T>();
    } else if constexpr (std::is_floating_point_v<T>) {
        return (s7_is_real(val))
               ? optional<T>(static_cast<T>(s7_real(val)))
               : optional<T>();
    } else if constexpr (std::is_same_v<const char*, T>) {
        return (s7_is_string(val))
            ? optional<T>(s7_string(val))
            : optional<T>();
    } else if constexpr (std::is_pointer_v<T>) {
        return (s7_is_c_pointer_of_type(
                    val,
                    s7_make_symbol(
                        scheme,
                        typeid(typename std::remove_pointer_t<T>).name())))
               ? optional<T>(static_cast<T>(s7_c_pointer(val)))
               : optional<T>();
    } else if constexpr (HAS_STATIC_METHOD(T, optional<T>,
                                           from_scheme,
                                           Scheme&, s7_pointer)) {
        return T::from_scheme(*this, val);
    } else {
        static_assert(dependent_false<T>::value,
                      "Unsupported conversion from Scheme type");
    }
}

template<typename T>
inline s7_pointer Scheme::into(T val) {
    if constexpr (std::is_same_v<s7_pointer, T>) {
        return val;
    } else if constexpr (std::is_integral_v<T>) {
        if constexpr (std::is_same_v<bool, T>) {
            return s7_make_boolean(scheme, val);
        } else {
            return s7_make_integer(scheme, val);
        }
    } else if constexpr (std::is_enum_v<T>) {
        return s7_make_integer(scheme, static_cast<int>(val));
    } else if constexpr (std::is_floating_point_v<T>) {
        return s7_make_real(scheme, val);
    } else if constexpr (std::is_same_v<const char*, T>) {
        return s7_make_string(scheme, val);
    } else if constexpr (std::is_pointer_v<T>) {
        return s7_make_c_pointer_with_type(
            scheme,
            static_cast<void*>(val),
            s7_make_symbol(scheme,
                           typeid(typename std::remove_pointer_t<T>).name()),
            s7_f(scheme));
    } else if constexpr (HAS_METHOD(T, s7_pointer, into_scheme, Scheme&)) {
        return val.into_scheme(*this);
    } else {
        static_assert(dependent_false<T>::value,
                      "Unsupported conversion into Scheme type");
    }
}


template<typename T>
s7_pointer Scheme::list(const vector<T>& vec) {
    s7_pointer prev = s7_nil(scheme);
    auto begin = vec.rbegin();
    auto end = vec.rend();
    for (; begin != end; ++begin) {
        prev = cons(*begin, prev);
    }
    return prev;
}

template<typename... TY>
inline tuple<optional<TY>...> Scheme::to_tuple(s7_pointer list) {
    return to_tuple_impl<remove_optional_t<TY>...>(
        list,
        std::make_index_sequence<sizeof...(TY)>());
}

#endif // ifndef SCHEME_HPP
