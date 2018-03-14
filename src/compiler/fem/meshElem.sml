structure meshElem =
    struct

    datatype mesh
        = UnitSquareMesh
        | SquareMesh
	| UnitCubeMesh
        | CubeMesh
        | None

    fun sameMesh(UnitSquareMesh, UnitSquareMesh) = true
      | sameMesh(SquareMesh, SquareMesh) = true
      | sameMesh (UnitCubeMesh,UnitCubeMesh) = true
      | sameMesh(CubeMesh, CubeMesh) = true
      | sameMesh(None, None) = true
      | sameMesh _ = false

    fun hashMesh (UnitSquareMesh) = 0w1
      | hashMesh (SquareMesh) = 0w2
      | hashMesh (UnitCubeMesh) = 0w3
      | hashMesh (CubeMesh) = 0w5
      | hashMesh (None) = 0w7

    fun toStringMesh (UnitSquareMesh) = "UnitSquareMesh"
     |  toStringMesh (SquareMesh) = "SquareMesh"
     | toStringMesh (UnitCubeMesh) = "UnitCubeMesh"
     | toStringMesh (CubeMesh) = "CubeMesh"
     | toStringMesh (None) = ""

    fun dimOfMesh (UnitSquareMesh) = 2
      | dimOfMesh (SquareMesh) = 2
      | dimOfMesh (UnitCubeMesh) = 3
      | dimOfMesh (CubeMesh) = 3
      | dimOfMesh (None) = raise Fail "none"

    datatype element = Lagrange | P

    fun sameElement(Lagrange,Lagrange) = true
      | sameElement(P, P) = true
      | sameElement _ =false

    fun hashElement (Lagrange) = 0w1
    | hashElement (P) = 0w3

    fun toStringElement (Lagrange) = "Lagrange"
      | toStringElement (P) = "P"
    fun toStringME(mesh, element) = String.concat[toStringMesh mesh, "_", toStringElement element]



    datatype fnspace = Space of mesh*element*int
    fun hashint i = Word.fromInt i

    fun samefnspace(Space(m1,e1,d1),Space(m2,e2,d2)) =
      sameMesh(m1,m2) andalso sameElement(e1,e2) andalso (d1=d2)
    fun hashfnspace(Space(m,e,d)) = hashMesh(m)+hashElement(e)+hashint d
    fun toStringfnspace(Space(m,e,d)) =
        String.concat[toStringMesh m, "_", toStringElement e,"_",Int.toString(d)]



    end
