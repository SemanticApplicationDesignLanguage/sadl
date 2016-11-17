/************************************************************************
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.processing

import com.google.common.base.Preconditions
import com.google.common.base.Supplier
import com.google.common.base.Suppliers
import com.google.common.collect.ImmutableMap
import com.google.common.collect.ImmutableSet
import com.google.common.collect.Maps
import com.google.inject.Injector
import com.google.inject.Provider
import java.util.Map
import java.util.ServiceLoader
import org.eclipse.emf.ecore.resource.Resource
import org.slf4j.LoggerFactory

/**
 * Abstract base class for all processor provider service classes. This
 * processor provider class should be used when no Eclipse platform is running,
 * hence extension-point based service discovery is not available at all. This
 * class uses the Java SPI discovery approach instead to load 3rd party
 * processor implementations.
 * 
 * @author akos.kitta
 */
abstract class AbstractSadlProcessorProvider<P> {

	static val LOGGER = LoggerFactory.getLogger(AbstractSadlProcessorProvider);

	val Supplier<Map<Class<? extends P>, Provider<P>>> processors;

	/**
	 * Creates a new provider instance with the class of the processors this
	 * instance provides. This class lazily discovers all service
	 * implementations.
	 */
	protected new(Class<P> processorClass, Injector injector) {
		processors = Suppliers.memoize [
			val builder = ImmutableMap.<Class<? extends P>, Provider<P>>builder;
			val services = ServiceLoader.load(processorClass).iterator;
			services.forEach [ instance |
				injector.injectMembers(instance);
				val Provider<P> provider = [instance];
				builder.put(instance.class as Class<? extends P>, provider);
			];
			return Maps.newHashMap(builder.build);
		];
	}

	/**
	 * Returns with the processor for the given resource argument.
	 */
	protected def P getProcessor(Resource resource);

	/**
	 * Returns with a view of all available processor instances.
	 */
	def Iterable<P> getAllProcessors() {
		ImmutableSet.copyOf(processors.get.values.map[get]);
	}

	/**
	 * Registers a new processor into the underlying cache. If already
	 * registered processor instance will be discarded and the argument will be
	 * registered instead.
	 * 
	 * @param processorProvider
	 *            the provider that provides the new processor instances. Cannot be {@code null}.
	 */
	def registerProcessor(Provider<P> processorProvider) {
		Preconditions.checkNotNull(processorProvider, 'processorProvider');
		val newProcessor = processorProvider.get;
		val key = newProcessor.class;
		val oldProcessor = processors.get.put(key as Class<? extends P>, processorProvider);
		if (oldProcessor === null) {
			LOGGER.info('''Processor has been successfully registered into the cache with class: '«key»'.''');
		} else {
			LOGGER.
				info('''Processor has been updated for class: '«key»'. New implementation is '«newProcessor»'. Old implementation was '«oldProcessor»'.''')
		}
	}

}
