/*
 * STDARG -- stdarg.h
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef __STDARG_H
#define __STDARG_H

typedef struct {
    unsigned gp_offset;
    unsigned fp_offset;
    void*    overflow_arg_area;
    void*    reg_save_area;
} __va_list;

typedef __va_list va_list[1];
typedef va_list   __gnuc_va_list;

#define va_start(LIST, LAST_ARG)                                                                   \
    do {                                                                                           \
        *(LIST) = *(__va_list*)__va__;                                                             \
    } while (0)

#define va_end(LIST)

static void* __va_arg_memory(__va_list* __list, __usize __size) {
    void* __ret               = __list->overflow_arg_area;
    __list->overflow_arg_area = (void*)(((__usize)__ret + __size + 7) & ~7);
    return __ret;
}

static void* __va_arg_integer(__va_list* __list, __usize __size) {
    if (__list->gp_offset >= 48)
        return __va_arg_memory(__list, __size);

    void* __ret = (__u8*)__list->reg_save_area + __list->gp_offset;
    __list->gp_offset += 8;
    return __ret;
}

#define va_arg(LIST, TY) (*(TY*)(__va_arg_integer((LIST), sizeof(TY))))

#endif
