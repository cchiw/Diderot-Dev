structure meshElem =
    struct

    datatype mesh
        = UnitSquareMesh
        | SquareMesh
	| UnitCubeMesh
        | CubeMesh
        | NoneMesh


    fun sameMesh(UnitSquareMesh, UnitSquareMesh) = true
      | sameMesh(SquareMesh, SquareMesh) = true
      | sameMesh (UnitCubeMesh,UnitCubeMesh) = true
      | sameMesh(CubeMesh, CubeMesh) = true
      | sameMesh(NoneMesh, NoneMesh) = true
      | sameMesh _ = false

    fun hashMesh (UnitSquareMesh) = 0w1
      | hashMesh (SquareMesh) = 0w2
      | hashMesh (UnitCubeMesh) = 0w3
      | hashMesh (CubeMesh) = 0w5
      | hashMesh (NoneMesh) = 0w7

    fun toStringMesh (UnitSquareMesh) = "UnitSquareMesh"
     |  toStringMesh (SquareMesh) = "SquareMesh"
     | toStringMesh (UnitCubeMesh) = "UnitCubeMesh"
     | toStringMesh (CubeMesh) = "CubeMesh"
     | toStringMesh (NoneMesh) = "NoneMesh"


    datatype element = Lagrange | P | NoneE

    fun sameElement(Lagrange,Lagrange) = true
      | sameElement(P, P) = true
      | sameElement(NoneE, NoneE) = true
      | sameElement _ =false

    fun hashElement (Lagrange) = 0w1
    | hashElement (P) = 0w3
    | hashElement (NoneE) = 0w5

    fun toStringElement (Lagrange) = "Lagrange"
      | toStringElement (P) = "P"
      | toStringElement (NoneE) = "NoneE"
    fun toStringME(mesh, element) = String.concat[toStringMesh mesh, "_", toStringElement element]



    datatype fnspace = Space of int*mesh*element*int
                        (*dimension, mesh, element, degree*)
    fun hashint i = Word.fromInt i

    fun samefnspace(Space(d1,m1,e1,o1),Space(d2,m2,e2,o2)) =
      sameMesh(m1,m2) andalso sameElement(e1,e2) andalso (d1=d2)andalso (o1=o2)
    fun hashfnspace(Space(d,m,e,o1)) = hashMesh(m)+hashElement(e)+hashint(d)+hashint( o1)

    fun toStringfnspace(Space(d,m,e,o1)) =
        String.concat["Dim",Int.toString(d),toStringMesh m, "_", toStringElement e,"_",Int.toString(o1)]



    end
