package dotty.dokka

import dotty.dokka.model._
import dotty.dokka.model.api._

class InheritanceInformationTransformer(using DocContext) extends (Module => Module):
  override def apply(original: Module): Module =
    val subtypes = getSupertypes(original.rootPackage).groupBy(_._1).transform((k, v) => v.map(_._2))
    original.updateMembers { m =>
      val st: Seq[LinkToType] = subtypes.getOrElse(m.dri, Nil)
      val rootMemberWithBareClasslikeKind = m.asLink.copy(kind = bareClasslikeKind(m.kind))
      m.withKnownChildren(st).withNewGraphEdges(st.map(_ -> rootMemberWithBareClasslikeKind))
    }

  private def bareClasslikeKind(kind: Kind): Kind = kind match
    case _: Kind.Trait => Kind.Trait(Nil, Nil)
    case _: Kind.Class => Kind.Class(Nil, Nil)
    case o => o

  private def getSupertypes(c: Member): Seq[(DRI, LinkToType)] =
    val selfMapping =
      if !c.kind.isInstanceOf[Classlike] then Nil
      else c.parents.map(_._2 -> c.asLink)
    c.members.flatMap(getSupertypes) ++ selfMapping
