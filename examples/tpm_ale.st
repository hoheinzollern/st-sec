Problem: TPM;
Principals: TPM, Alice, KeyStore;
Functions: enc/2, dec/2, CreateWrapKey/3 {data}, Seal/3 {data}, Unseal/1 {data}, Extend/1 {data}, if/3, true, false;
Equations: dec(enc(x,y),y) = x, if(true, x, y) = x, if(false, x, y) = y;

Protocol:

let TPMProto(x @ Alice, v_pcr @ TPM, id_tpm @ TPM, tpm_proof @ TPM) =

  Alice -> TPM: match x with {
    CreateWrapKey(x_pk, ty, v_lock):
      TPM { new key; }
      KeyStore ->* TPM: <x_id_tpm, x_sk, x_pk', ty', v_pcr'>
      TPM { let msg = if(
        x_id_tpm = id_tpm & x_pk = x_pk' & (v_pcr' = nil | v_pcr = v_pcr'),
        <pk(key), wrap(x_pk, key, ty, tpm_proof, v_lock)>,
        fail
      ); }
      TPM -> Alice: <msg>
      TPMProto(v_pcr, id_tpm, tpm_proof)

    Seal(d, x_pcr, x_pk):
      KeyStore ->* TPM: <x_id_tpm, x_sk, x_pk', ty, v_pcr'>
      TPM { let msg = IfThenElse(
        x_id_tpm = id_tpm & x_pk' = x_pk & ty = sealk &
          (v_pcr' = nil | v_pcr = v_pcr'),
        seal(x_pk, d, tpm_proof, x_pcr),
        fail
      ); }
      TPM -> Alice: <msg>
      TPMProto(v_pcr, id_tpm, tpm_proof)

    Unseal(x):
      TPM { let <x_pk, d, tpm_proof', v_pcr'> = unseal(x); }
      KeyStore ->* TPM: <x_id_tpm, x_sk, x_pk', ty, v_pcr''>
      TPM { let msg = IfThenElse(
        x_id_tpm = id_tpm & x_pk' = x_pk & ty = sealk &
          ((v_pcr' = nil & v_pcr'' = nil) | (v_pcr' = nil & v_pcr = v_pcr'') | (v_pcr' = v_pcr & v_pcr'' = nil) | (v_pcr' = v_pcr & v_pcr'' = v_pcr)),
        d,
        fail
      ); }
      TPM -> Alice: <msg>
      TPMProto(v_pcr, id_tpm, tpm_proof)

    Extend(x):
      TPM { let new_pcr = hpcr(v_pcr, x); }
      TPMProto(new_pcr, id_tpm, tpm_proof)
  }

in

let TPMInit() =
  TPM *-> KeyStore: <x_id_tpm, x_sk, x_pk', ty, v_pcr'>
  TPMProto(init_pcr, id_tpm, tpm_proof)

in TPMInit()
