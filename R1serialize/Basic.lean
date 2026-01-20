import Mathlib.Tactic

def hello := "world"

private def toBytes (n size : ℕ) : List UInt8 :=
  match size with
  | 0 => []
  | size' + 1 =>
    let byte : UInt8 := ⟨n % UInt8.size, Nat.mod_lt _ Nat.ofNat_pos⟩
    byte :: toBytes (n / UInt8.size) size'

private def Nat.toByteArray (n size : ℕ) : ByteArray :=
  List.toByteArray (toBytes n size)

example : Nat.toByteArray (size := 0) 123 = ⟨#[]⟩ := by rfl
example : Nat.toByteArray (size := 1) 0 = ⟨#[0]⟩ := by rfl
example : Nat.toByteArray (size := 5) 0 = ⟨#[0, 0, 0, 0, 0]⟩ := by rfl
example : Nat.toByteArray (size := 3) (2^8-1) = ⟨#[255, 0, 0]⟩ := by rfl
example : Nat.toByteArray (size := 3) (2^8) = ⟨#[0, 1, 0]⟩ := by rfl
example : Nat.toByteArray (size := 3) (2^16) = ⟨#[0, 0, 1]⟩ := by rfl

private def UInt32.toByteArray (i : UInt32) : ByteArray := i.toNat.toByteArray 4

example : (0 : UInt32).toByteArray = ⟨#[0, 0, 0, 0]⟩ := by rfl
example : (255 : UInt32).toByteArray = ⟨#[255, 0, 0, 0]⟩ := by rfl
example : (256 : UInt32).toByteArray = ⟨#[0, 1, 0, 0]⟩ := by rfl
example : (257 : UInt32).toByteArray = ⟨#[1, 1, 0, 0]⟩ := by rfl

private def UInt64.toByteArray (i : UInt64) : ByteArray := i.toNat.toByteArray 8

example : (0 : UInt64).toByteArray = ⟨#[0, 0, 0, 0, 0, 0, 0, 0]⟩ := by rfl
example : (255 : UInt64).toByteArray = ⟨#[255, 0, 0, 0, 0, 0, 0, 0]⟩ := by rfl
example : (256 : UInt64).toByteArray = ⟨#[0, 1, 0, 0, 0, 0, 0, 0]⟩ := by rfl
example : (257 : UInt64).toByteArray = ⟨#[1, 1, 0, 0, 0, 0, 0, 0]⟩ := by rfl

def magic : ByteArray := "r1cs".bytes
def version : UInt32 := 1
def nSections : UInt32 := 3

structure Header where
  fieldElemSize : UInt32
  prime : ℕ
  nWires : UInt32
  nPubOut : UInt32
  nPubIn : UInt32
  nPrvIn : UInt32
  nLabels : UInt64
  mConstraints : UInt32

def Header.size (h : Header) : UInt64 :=
  UInt64.ofNat <| 6 * 4 + h.fieldElemSize.toNat + 8

def Header.toByteArray (h : Header) : ByteArray :=
  Array.foldl .append .empty
  #[
    h.fieldElemSize.toByteArray,
    h.prime.toByteArray h.fieldElemSize.toNat,
    h.nWires.toByteArray,
    h.nPubOut.toByteArray,
    h.nPubIn.toByteArray,
    h.nPrvIn.toByteArray,
    h.nLabels.toByteArray,
    h.mConstraints.toByteArray
  ]

structure Constraint where
  nA : UInt32
  nB : UInt32
  nC : UInt32
  termsA : Array (UInt32 × ℕ)
  termsB : Array (UInt32 × ℕ)
  termsC : Array (UInt32 × ℕ)

def Constraint.size (h : Header) (c : Constraint) : UInt64 :=
  UInt64.ofNat <|
    3 * 4 + (c.termsA.size + c.termsB.size + c.termsC.size) * (4 + h.fieldElemSize.toNat)

def Constraint.toByteArray (h : Header) (c : Constraint) : ByteArray :=
  printTerms c.termsA c.nA
    ++ printTerms c.termsB c.nB
    ++ printTerms c.termsC c.nC
 where
  printTerms (terms : Array (UInt32 × ℕ)) (size : UInt32) : ByteArray :=
    let sizeBA := size.toByteArray
    let terms := terms.take size.toNat
    let termsBAs :=
      terms.map fun (id, coeff) ↦
        id.toByteArray ++ coeff.toByteArray h.fieldElemSize.toNat
    sizeBA ++ termsBAs.foldl .append .empty

abbrev Constraints := Array Constraint

def Constraints.size (h : Header) (cs : Constraints) : UInt64 :=
  (cs.map (Constraint.size h)).sum

def Constraints.toByteArray (h : Header) (cs : Constraints) : ByteArray :=
  let cs := cs.take h.mConstraints.toNat
  let csBA := cs.map fun c ↦ c.toByteArray h
  csBA.foldl .append .empty

abbrev WireToLabelMap := Array UInt64

def WireToLabelMap.size (wMap : WireToLabelMap) : UInt64 :=
  UInt64.ofNat <| 8 * Array.size wMap

def WireToLabelMap.toByteArray (h : Header) (wMap : WireToLabelMap) : ByteArray :=
  let wMap := wMap.take h.nLabels.toNat
  let wMapBAs := wMap.map fun l ↦ l.toByteArray
  wMapBAs.foldl .append .empty

structure R1CSv1 where
  header : Header
  constraints : Constraints
  wireToLabelMap : WireToLabelMap
  ultraPLONKCustomGateList : Unit
  ultraPLONKCustomGateApplication : Unit

def serializeR1CS (path : System.FilePath) (r1cs : R1CSv1) : IO Unit := do
  let h ← IO.FS.Handle.mk path IO.FS.Mode.write
  h.write magic
  h.write version.toByteArray
  h.write nSections.toByteArray
  -- Section 2
  h.write (2 : UInt32).toByteArray
  h.write (r1cs.constraints.size r1cs.header).toByteArray
  h.write (r1cs.constraints.toByteArray r1cs.header)
  -- Section 1
  h.write (1 : UInt32).toByteArray
  h.write r1cs.header.size.toByteArray
  h.write r1cs.header.toByteArray
  -- Section 3
  h.write (3 : UInt32).toByteArray
  h.write r1cs.wireToLabelMap.size.toByteArray
  h.write (r1cs.wireToLabelMap.toByteArray r1cs.header)
  h.flush
