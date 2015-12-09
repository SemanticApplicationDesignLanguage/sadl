/** 
 */
package com.ge.research.sadl.generator

import static com.google.common.collect.Sets.newHashSet
import java.util.Set
import org.eclipse.xtext.generator.IFileSystemAccess
import org.eclipse.xtext.generator.OutputConfiguration
import org.eclipse.xtext.generator.OutputConfigurationProvider

/** 
 * @author 210060824
 */
class SADLOutputConfigurationProvider extends OutputConfigurationProvider {
	override Set<OutputConfiguration> getOutputConfigurations() {
		var OutputConfiguration defaultOutput = new OutputConfiguration(IFileSystemAccess.DEFAULT_OUTPUT)
		defaultOutput.setDescription("Output Folder")
		defaultOutput.setOutputDirectory("./OwlModels")
		defaultOutput.setOverrideExistingResources(true)
		defaultOutput.setCreateOutputDirectory(true)
		defaultOutput.setCleanUpDerivedResources(true)
		defaultOutput.setSetDerivedProperty(true)
		defaultOutput.setKeepLocalHistory(true)
		return newHashSet(defaultOutput)
	}
}
