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

#ifndef C_SNARK_H
#define C_SNARK_H

#include "common.h"

#ifdef __cplusplus
extern "C" {
#endif

#define scalar_len 32
#define point_len 64
#define pair_len 32 * 2 + 64 * 2 // each pair in alt_bn128_pairing_product

// For details of the interfaces, see Snark.h. All memory for the
// buffers below must be allocated by the caller. All functions
// return true on success and false on failure.

// "result" must be allocated to be of size scalar_len.
// "pairs"'s length must be a multiple of pair_len.
bool alt_bn128_pairing_product_Z(const RawBytes_Z* pairs, RawBytes_Z *result);
// "result" must be allocated to be of size point_len
// "p1" must have length point_len and "s" scalar_len
bool alt_bn128_G1_mul_Z(const RawBytes_Z* p1, const RawBytes_Z* s, RawBytes_Z* result);
// "result" must be allocated to be of size point_len
// "p1" and "p2" must have length point_len.
bool alt_bn128_G1_add_Z(const RawBytes_Z* p1, const RawBytes_Z* p2, RawBytes_Z* result);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // C_SNARK_H