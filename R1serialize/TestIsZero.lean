import R1serialize.Basic

private def header : Header where
  fieldElemSize := 32
  prime := 21888242871839275222246405745257275088548364400416034343698204186575808495617
  nWires := 4
  nPubOut := 1
  nPubIn := 0
  nPrvIn := 1
  nLabels := 4
  mConstraints := 2

private def constraints : Constraints :=
  #[
    { nA := 1
      nB := 1
      nC := 2
      termsA := #[(2, 1)]
      termsB := #[(3, 1)]
      termsC :=
        #[
          (0, 1),
          (1, 21888242871839275222246405745257275088548364400416034343698204186575808495616)
          ]
    },
    { nA := 1
      nB := 1
      nC := 0
      termsA := #[(2, 1)]
      termsB := #[(1, 1)]
      termsC := #[]
    },
  ]

private def wireToLabelMap : WireToLabelMap := #[0, 1, 2, 3]

private def isZero : R1CSv1 where
  header := header
  constraints := constraints
  wireToLabelMap := wireToLabelMap
  ultraPLONKCustomGateApplication := ()
  ultraPLONKCustomGateList := ()

private def debug : IO Unit := do
  let original ← IO.FS.readBinFile "R1CS/isZero.r1cs"
  let path := "R1CS/out.r1cs"
  serializeR1CS path isZero
  let output ← IO.FS.readBinFile path
  IO.println <| "original size: " ++ toString original.size
  IO.println <| "output size: " ++ toString output.size
  IO.println <| original == output


#eval debug
