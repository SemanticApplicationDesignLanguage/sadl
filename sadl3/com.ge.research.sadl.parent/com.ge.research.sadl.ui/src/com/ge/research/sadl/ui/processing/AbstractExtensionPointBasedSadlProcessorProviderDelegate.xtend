/************************************************************************
 * Copyright 2007-2016- General Electric Company, All Rights Reserved
 * 
 * Project: SADL
 * 
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language.
 * 
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 * 
 ***********************************************************************/
package com.ge.research.sadl.ui.processing

import com.ge.research.sadl.processing.AbstractSadlProcessorProvider
import com.google.common.base.Supplier
import com.google.common.base.Suppliers
import com.google.inject.Inject
import com.google.inject.Injector
import org.eclipse.core.runtime.Platform
import org.eclipse.core.runtime.RegistryFactory

/**
 * Generic, Eclipse extension-point based processor provider implementation.
 * Unlike the {@link AbstractSadlProcessorProvider} this implementation uses extension-points
 * for the discovery of the concrete implementations, hence it requires a running
 * {@link Platform platform}.
 * 
 * @author akos.kitta
 */
abstract class AbstractExtensionPointBasedSadlProcessorProviderDelegate<P> {
	
	val Supplier<Iterable<P>> processors;

	@Inject	
	protected new(Class<? extends P> processorClass, Injector injector) {
		processors = Suppliers.memoize[
			val registry = RegistryFactory.getRegistry();
			registry.getConfigurationElementsFor(extensionPointId).map[
				val processor = processorClass.cast(createExecutableExtension(classPropertyName));
				// XXX: why not use AbstractGuiceAwareExecutableExtensionFactory instead?
				injector.injectMembers(processor);
				return processor;
			];		
		];
	}
	
	def Iterable<P> getAllProviders() {
		return processors.get;
	}
	
	/**
	 * Returns with the unique extension-point ID of the processor that has to 
	 * be discovered by this provider.
	 */
	abstract protected def String getExtensionPointId();

	/**
	 * Returns with the property name that has to be sued to create the executable
	 * extension from the configuration element of the extension point.
	 * 
	 * <p>
	 * By default it returns with the {@code class} string. Clients may change it.
	 */	
	protected def String getClassPropertyName() {
		return 'class';
	}
	
}