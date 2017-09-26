package org.jetbrains.plugins.scala
package lang
package refactoring
package introduceVariable

import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement

case class OccurrenceData private(usualOccurrences: Seq[ScTypeElement],
                                  companionObjectOccurrences: Seq[ScTypeElement],
                                  extendedClassOccurrences: Seq[ScTypeElement]) {

  def allOccurrences: Seq[ScTypeElement] = usualOccurrences ++
    companionObjectOccurrences ++ extendedClassOccurrences
}

object OccurrenceData {

  def apply(scopeItem: ScopeItem,
            isReplaceAllUsual: Boolean = true,
            isReplaceOccurrenceInCompanionObject: Boolean = false,
            isReplaceOccurrenceInInheritors: Boolean = false): OccurrenceData = {
    val (usualOccurrences, occurrencesInCompanion, occurrencesFromInheritors) =
      scopeItem match {
        case simpleScope: SimpleScopeItem =>
          (simpleScope.usualOccurrences,
            simpleScope.occurrencesInCompanion,
            simpleScope.occurrencesFromInheritors)
        case packageScope: PackageScopeItem =>
          (packageScope.occurrences, Array.empty, Array.empty)
      }

    new OccurrenceData(
      if (isReplaceAllUsual) usualOccurrences else Seq.empty,
      if (isReplaceOccurrenceInCompanionObject) occurrencesInCompanion else Seq.empty,
      if (isReplaceOccurrenceInInheritors) occurrencesFromInheritors else Seq.empty)

  }
}
