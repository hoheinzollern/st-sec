Principals: TPM, Alice, KeyStore;

Protocol:

let TPMProto(v_pcr @ TPM, id_tpm @ TPM, tpm_proof @ TPM) =

  Alice -> TPM: {
    CreateWrapKey(x_pk, ty, v_lock):
      TPM { new key; }
      KeyStore o->* TPM: <x_id_tpm, x_sk, x_pk', ty', v_pcr'>;
      TPM { let msg = IfThenElse(
        x_id_tpm = id_tpm && x_pk = x_pk' && (v_pcr' = nil || v_pcr = v_pcr'),
        <pk(key), wrap(x_pk, key, ty, tpm_proof, v_lock)>,
        fail
      ); }
      TPM -> Alice: <msg>;
      TPMProto(v_pcr, id_tpm, tpm_proof)

    Seal(data, x_pcr, x_pk):
      KeyStore o->* TPM: <x_id_tpm, x_sk, x_pk', ty, v_pcr'>;
      TPM { let msg = IfThenElse(
        x_id_tpm = id_tpm && x_pk' = x_pk && ty = sealk &&
          (v_pcr' = nil || v_pcr = v_pcr'),
        seal(x_pk, data, tpm_proof, x_pcr),
        fail
      ); }
      TPM -> Alice: <msg>;
      TPMProto(v_pcr, id_tpm, tpm_proof)

    Unseal(x):
      TPM { let <x_pk, data, tpm_proof', v_pcr'> = unseal(x); }
      KeyStore o->* TPM: <x_id_tpm, x_sk, x_pk', ty, v_pcr''>;
      TPM { let msg = IfThenElse(
        x_id_tpm = id_tpm && x_pk' = x_pk && ty = sealk &&
          ((v_pcr' = nil && v_pcr'' = nil) || (v_pcr' = nil && v_pcr = v_pcr'') || (v_pcr' = v_pcr && v_pcr'' = nil) || (v_pcr' = v_pcr && v_pcr'' = v_pcr)),
        data,
        fail
      ); }
      TPM -> Alice: <msg>;
      TPMProto(v_pcr, id_tpm, tpm_proof)

    Extend(x):
      TPM { let new_pcr = hpcr(v,x); }
      TPMProto(new_pcr, id_tpm, tpm_proof)
  }

in

let TPMInit() =
  TPM { new....; }
  TPM *->o KeyStore: <x_id_tpm, x_sk, x_pk', ty, v_pcr'>;
  TPMProto(init_pcr, id_tpm, tpm_proof)

in TPMInit()