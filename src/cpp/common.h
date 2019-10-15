/*
 * Copyright (C) 2019 Zilliqa
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#ifndef COMMON_H
#define COMMON_H

#include <cstdio>
#include <cstdlib>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct
{
    char* data;
    int len;
} RawBytes_Z;

// OCaml CTypes does not support handling exceptions. So just abort.
static void err_abort(const char* msg)
{
    fprintf(stderr, "%s\n", msg);
    abort();
}

#ifdef __cplusplus
} // extern "C"
#endif

#endif // COMMON_H
