import R1serialize.Basic

structure Witness where
  n8 : UInt32
  prime : ℕ
  nVars : UInt32
  elements : Array ℕ

namespace Witness

def magic : ByteArray := "wtns".bytes
def version : UInt32 := 2
def nSections : UInt32 := 2

def section1Length (w : Witness) : UInt64 :=
  UInt64.ofNat <| 4 + w.n8.toNat + 4

def section2Length (w : Witness) : UInt64 :=
  UInt64.ofNat <| w.n8.toNat * w.nVars.toNat

def serializeWitness (path : System.FilePath) (w : Witness) : IO Unit := do
  let h ← IO.FS.Handle.mk path IO.FS.Mode.write
  h.write magic
  h.write version.toByteArray
  h.write nSections.toByteArray
  -- Section 1
  h.write (1 : UInt32).toByteArray
  h.write w.section1Length.toByteArray
  h.write w.n8.toByteArray
  h.write (w.prime.toByteArray w.n8.toNat)
  h.write w.nVars.toByteArray
  -- Section 2
  h.write (2 : UInt32).toByteArray
  h.write w.section2Length.toByteArray
  let elements :=
    w.elements.take w.nVars.toNat |>.map (Nat.toByteArray · w.n8.toNat)
  h.write (elements.foldl .append .empty)
  h.flush

private def wtns : Witness where
  n8 := 32
  prime := 21888242871839275222246405745257275088548364400416034343698204186575808495617
  nVars := 4
  elements := #[1, 6, 2, 3]

private def debug : IO Unit := do
  let original ← IO.FS.readBinFile "Witness/mul.wtns"
  let path := "Witness/out.wtns"
  serializeWitness path wtns
  let output ← IO.FS.readBinFile path
  IO.println <| "original size: " ++ toString original.size
  IO.println <| "output size: " ++ toString output.size
  IO.println <| original == output

#eval debug

end Witness
