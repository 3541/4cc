/*
 * UTIL -- Miscellaneous utilities.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 */

#include "util.h"

size_t util_ident(void) {
    static size_t I = 0;

    return I++;
}
