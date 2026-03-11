// SPDX-License-Identifier: Apache-2.0

package chisel3

import chisel3.experimental.{BaseModule, SourceInfo}
import chisel3.util.simpleClassName

/** This package contains Chisel language definitions for describing configuration options and their accepted values.
  */
package object choice {

  /** An option group declaration. Specifies a container grouping values for some design configuration parameter.
    *
    * @example
    * {{{
    * import chisel3.option.{Group, Case}
    * object Platform extends Group {
    *   object FPGA extends Case
    *   object ASIC extends Case
    * }
    * }}}
    */
  abstract class Group(implicit _sourceInfo: SourceInfo) {
    self: Singleton =>

    private[chisel3] def sourceInfo: SourceInfo = _sourceInfo

    private[chisel3] def name: String = simpleClassName(this.getClass())

    final implicit def group: Group = this
  }

  /** A dynamic option group declaration that accepts a name and list of case names as String parameters.
    * This allows creating groups at runtime without requiring singleton objects.
    *
    * If a DynamicGroup with the same name already exists in the current elaboration context,
    * that existing group will be returned instead of creating a new one.
    *
    * @param groupName The name of the group
    * @param caseNames List of case names for this group
    * @param _sourceInfo Source location information
    *
    * @example
    * {{{
    * import chisel3.choice.DynamicGroup
    *
    * val platform = new DynamicGroup("Platform", Seq("FPGA", "ASIC"))
    * // Access cases: platform.cases("FPGA"), platform.cases("ASIC")
    * }}}
    */
  class DynamicGroup(val groupName: String, caseNames: Seq[String])(implicit _sourceInfo: SourceInfo) {
    import chisel3.internal.Builder

    private[chisel3] def sourceInfo: SourceInfo = _sourceInfo

    private[chisel3] def name: String = groupName

    // Check if a group with this name already exists and validate case names match
    if (Builder.inContext) {
      Builder.getDynamicGroupInfo(groupName) match {
        case Some((existingGroup, existingCaseNames)) =>
          // Group exists, verify case names match exactly (including order)
          if (existingCaseNames != caseNames) {
            throw new IllegalArgumentException(
              s"Error: DynamicGroup '$groupName' already exists with different case names.\n" +
              s"  Existing cases: ${existingCaseNames.mkString(", ")}\n" +
              s"  New cases: ${caseNames.mkString(", ")}"
            )
          }
        case None =>
          // Group doesn't exist yet, will be created below
      }
    }

    // Create a factory that produces a singleton Group object
    private def createGroupFactory(): () => Group = () => {
      object DynamicGroupSingleton extends Group()(_sourceInfo) {
        override private[chisel3] def name: String = groupName
      }
      DynamicGroupSingleton
    }

    // Get or create the singleton Group for this name from the Builder context
    // All DynamicGroups with the same name will share the same Group singleton
    private val _group: Group =
      if (Builder.inContext) {
        Builder.getOrCreateDynamicGroup(groupName, caseNames, createGroupFactory())
      } else {
        // If not in context, just call the factory
        createGroupFactory()()
      }

    // Provide an implicit group - returns the shared singleton Group for this name
    final implicit def group: Group = _group

    // Create Case singletons for each case name
    private val _cases: Map[String, Case] = {
      caseNames.map { caseName =>
        val caseFactory = () => {
          object DynamicCaseSingleton extends Case()(_group, _sourceInfo) {
            override private[chisel3] def name: String = caseName
          }
          DynamicCaseSingleton
        }

        val caseObj = if (Builder.inContext) {
          Builder.getOrCreateDynamicCase(_group, caseName, caseFactory)
        } else {
          caseFactory()
        }

        caseName -> caseObj
      }.toMap
    }

    // Provide access to cases by name
    def cases: Map[String, Case] = _cases

    // Convenience method to get a case by name
    def apply(caseName: String): Case = _cases.getOrElse(caseName,
      throw new NoSuchElementException(s"Case '$caseName' not found in group '$groupName'. Available cases: ${_cases.keys.mkString(", ")}")
    )
  }

  object DynamicGroup {
    /** Create a DynamicGroup with the given name and case names.
      * If a group with this name already exists in the elaboration context,
      * the returned DynamicGroup will share the same underlying Group singleton.
      *
      * @param name The name of the group
      * @param caseNames List of case names for this group
      * @param sourceInfo Source location information
      * @return A DynamicGroup with the given name
      */
    def apply(name: String, caseNames: Seq[String])(implicit sourceInfo: SourceInfo): DynamicGroup = {
      new DynamicGroup(name, caseNames)
    }
  }

  /** An option case declaration.
    */
  abstract class Case(implicit val group: Group, _sourceInfo: SourceInfo) {
    self: Singleton =>

    private[chisel3] def sourceInfo: SourceInfo = _sourceInfo

    private[chisel3] def name: String = simpleClassName(this.getClass())

    /** A helper method to allow ModuleChoice to use the `->` syntax to specify case-module mappings.
      *
      * It captures a lazy reference to the module and produces a generator to avoid instantiating it.
      *
      * @param module Module to map to the current case.
      */
    def ->[T](module: => T): (Case, () => T) = (this, () => module)
  }

}
