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

#include "c_snark.h"
#include "Snark.h"

extern "C" {

bool alt_bn128_pairing_product_Z(const RawBytes_Z* pairs, RawBytes_Z *result)
{
  try {
    bytes pairs_b(pairs->data, pairs->data + pairs->len);
    bytes result_b = alt_bn128_pairing_product(pairs_b);
    std::copy(result_b.begin(), result_b.end(), result->data);
    return true;
  } catch (...) {
    return false;
  }
}

bool alt_bn128_G1_mul_Z(const RawBytes_Z* p1, const RawBytes_Z* s, RawBytes_Z* result)
{
  try {
    bytes p1_b(p1->data, p1->data + p1->len);
    bytes s_b(s->data, s->data + s->len);
    bytes result_b = alt_bn128_G1_mul(p1_b, s_b);
    std::copy(result_b.begin(), result_b.end(), result->data);
    return true;
  } catch (...) {
    return false;
  }
}

bool alt_bn128_G1_add_Z(const RawBytes_Z* p1, const RawBytes_Z* p2, RawBytes_Z* result)
{
  try {
    bytes p1_b(p1->data, p1->data + p1->len);
    bytes p2_b(p2->data, p2->data + p2->len);
    bytes result_b = alt_bn128_G1_add(p1_b, p2_b);
    std::copy(result_b.begin(), result_b.end(), result->data);
    return true;
  } catch (...) {
    return false;
  }
}

} // extern "C"
