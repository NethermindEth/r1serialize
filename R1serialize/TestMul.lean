import R1serialize.Basic

private def header₁ : Header where
  fieldElemSize := 32
  prime := 21888242871839275222246405745257275088548364400416034343698204186575808495617
  nWires := 4
  nPubOut := 1
  nPubIn := 0
  nPrvIn := 2
  nLabels := 4
  mConstraints := 1

private def constraints₁ : Constraints :=
  .singleton
    { nA := 1
      nB := 1
      nC := 1
      termsA :=
        #[(2, 21888242871839275222246405745257275088548364400416034343698204186575808495616)]
      termsB := #[(3, 1)]
      termsC :=
        #[(1, 21888242871839275222246405745257275088548364400416034343698204186575808495616)]
    }

private def wireToLabelMap₁ : WireToLabelMap := #[0, 1, 2, 3]

private def ex₁ : R1CSv1 where
  header := header₁
  constraints := constraints₁
  wireToLabelMap := wireToLabelMap₁
  ultraPLONKCustomGateApplication := .empty
  ultraPLONKCustomGateList := .empty

private def debug : IO Unit := do
  let original ← IO.FS.readBinFile "R1CS/mul.r1cs"
  let path := "test.r1cs"
  serializeR1CS path ex₁
  let output ← IO.FS.readBinFile path
  IO.println <| "original size: " ++ toString original.size
  IO.println <| "output size: " ++ toString output.size
  IO.println <| original == output
  -- IO.println <| original.extract l r
  -- IO.println <| output.extract l r

#eval debug
