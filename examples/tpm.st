Principals: TPM, Alice;
Public: pcr0;
Protocol:
let TPM_Envelope(id, srk, pk_srk, bindk, nil) =
  ....
  TPM_Envelope(.....)
in
let TPM_Init() =
  TPM {
    new aik; // Authentication identity key
    let pk_aik = pk(aik);
  }
  TPM -> Alice: pk_aik;
