Alice -> TPM: x = enc(y, z)
Alice -> TPM: match x with {
  CreateWrapKey(x_pk, ty, v_lock):
    TPM { new key; }
    KeyStore *->* TPM: x = <x_id_tpm, x_sk, x_pk', ty', v_pcr'>
    TPM { let msg = IfThenElse(
      x_id_tpm = id_tpm & x_pk = x_pk' & (v_pcr' = nil | v_pcr = v_pcr'),
      <pk(key), wrap(x_pk, key, ty, tpm_proof, v_lock)>,
      fail
    ); }
    TPM -> Alice: x = <msg>
    TPMProto(v_pcr, id_tpm, tpm_proof)
}
