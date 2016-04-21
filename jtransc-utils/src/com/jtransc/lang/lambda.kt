package com.jtransc.lang

inline fun <T> exec(callback: () -> T) = callback()