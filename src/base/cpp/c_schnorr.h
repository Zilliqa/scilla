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

#ifndef C_SCHNORR_H
#define C_SCHNORR_H

#include "common.h"

#ifdef __cplusplus
extern "C" {
#endif

#define privkey_len 32
#define pubkey_len 33
#define signature_len 64

// Generate a private/public key pair.
// Memory must already be allocated by caller.
bool genKeyPair_Z(RawBytes_Z* privKey, RawBytes_Z* pubKey);

// Sign message with privKey/pubKey. Memory for signature must be allocated by caller.
bool sign_Z(const RawBytes_Z* privKey, const RawBytes_Z* pubKey,
            const RawBytes_Z* message, RawBytes_Z* signature);

// Verify message with signature and public key of signer
bool verify_Z(const RawBytes_Z* pubKey, const RawBytes_Z* message,
             RawBytes_Z* signature, int* result);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // C_SCHNORR_H
