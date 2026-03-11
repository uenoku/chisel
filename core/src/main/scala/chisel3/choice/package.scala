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

  /** A dynamic option group declaration that accepts a name as a String parameter.
    * This allows creating groups at runtime without requiring singleton objects.
    * Cases can be defined as nested singleton objects within the DynamicGroup.
    *
    * If a DynamicGroup with the same name already exists in the current elaboration context,
    * that existing group will be returned instead of creating a new one.
    *
    * @param groupName The name of the group
    * @param _sourceInfo Source location information
    *
    * @example
    * {{{
    * import chisel3.choice.{DynamicGroup, Case}
    *
    * object Platform extends DynamicGroup("Platform") {
    *   object FPGA extends Case
    *   object ASIC extends Case
    * }
    *
    * // Or create dynamically:
    * val platform = new DynamicGroup("Platform")
    * object FPGA extends Case()(platform.group, implicitly)
    * }}}
    */
  class DynamicGroup(val groupName: String)(implicit _sourceInfo: SourceInfo) {
    import chisel3.internal.Builder

    private[chisel3] def sourceInfo: SourceInfo = _sourceInfo

    private[chisel3] def name: String = groupName

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
        Builder.getOrCreateDynamicGroup(groupName, createGroupFactory())
      } else {
        // If not in context, just call the factory
        createGroupFactory()()
      }

    // Provide an implicit group - returns the shared singleton Group for this name
    final implicit def group: Group = _group
  }

  object DynamicGroup {
    /** Create a DynamicGroup with the given name.
      * If a group with this name already exists in the elaboration context,
      * the returned DynamicGroup will share the same underlying Group singleton.
      *
      * @param name The name of the group
      * @param sourceInfo Source location information
      * @return A DynamicGroup with the given name
      */
    def apply(name: String)(implicit sourceInfo: SourceInfo): DynamicGroup = {
      new DynamicGroup(name)
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

  /** A dynamic option case declaration that accepts a name as a String parameter.
    * This allows creating cases at runtime without requiring singleton objects.
    *
    * If a Case with the same name already exists in the same group, it will be reused.
    *
    * @param caseName The name of the case
    * @param group The group this case belongs to
    * @param _sourceInfo Source location information
    *
    * @example
    * {{{
    * import chisel3.choice.{DynamicGroup, DynamicCase}
    * val platform = new DynamicGroup("Platform")
    * val fpga = new DynamicCase("FPGA", platform.group)
    * val asic = new DynamicCase("ASIC", platform.group)
    * }}}
    */
  class DynamicCase(val caseName: String, val caseGroup: Group)(implicit _sourceInfo: SourceInfo) {
    import chisel3.internal.Builder

    private[chisel3] def sourceInfo: SourceInfo = _sourceInfo

    private[chisel3] def name: String = caseName

    // Create a factory that produces a singleton Case object
    private def createCaseFactory(): () => Case = () => {
      object DynamicCaseSingleton extends Case()(caseGroup, _sourceInfo) {
        override private[chisel3] def name: String = caseName
      }
      DynamicCaseSingleton
    }

    // Get or create the singleton Case for this name and group
    private val _case: Case =
      if (Builder.inContext) {
        Builder.getOrCreateDynamicCase(caseGroup, caseName, createCaseFactory())
      } else {
        // If not in context, just call the factory
        createCaseFactory()()
      }

    /** A helper method to allow ModuleChoice to use the `->` syntax to specify case-module mappings.
      *
      * It captures a lazy reference to the module and produces a generator to avoid instantiating it.
      *
      * @param module Module to map to the current case.
      */
    def ->[T](module: => T): (Case, () => T) = (_case, () => module)

    // Expose the underlying Case
    def asCase: Case = _case
  }

  object DynamicCase {
    /** Create a DynamicCase with the given name and group.
      * If a case with this name already exists in the same group,
      * the returned DynamicCase will share the same underlying Case singleton.
      *
      * @param name The name of the case
      * @param group The group this case belongs to
      * @param sourceInfo Source location information
      * @return A DynamicCase with the given name
      */
    def apply(name: String, group: Group)(implicit sourceInfo: SourceInfo): DynamicCase = {
      new DynamicCase(name, group)
    }
  }
}
