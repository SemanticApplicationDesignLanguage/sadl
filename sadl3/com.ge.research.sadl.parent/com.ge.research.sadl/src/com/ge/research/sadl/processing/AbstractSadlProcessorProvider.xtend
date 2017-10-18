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

import com.google.common.base.Optional
import com.google.common.base.Preconditions
import com.google.common.base.Suppliers
import com.google.common.collect.ImmutableMap
import com.google.common.collect.ImmutableSet
import com.google.common.collect.Maps
import com.google.inject.Injector
import com.google.inject.Provider
import java.util.Map
import java.util.ServiceLoader
import org.eclipse.core.runtime.RegistryFactory
import org.eclipse.emf.common.EMFPlugin
import org.slf4j.LoggerFactory

/**
 * Abstract base class for all processor provider service classes. This
 * processor provider class could be used when the Eclipse platform is running,
 * hence extension-point based service discovery available. Besides that, when 
 * the Eclipse platform is not running, it uses the the Java SPI discovery 
 * approach instead to load 3rd party processor implementations.
 * 
 * @param 
 * 	<P> Type of the provided processor.
 * @param 
 * 	<R> Type of the subject object which is used to provide the processor.
 * 
 * @author akos.kitta
 */
abstract class AbstractSadlProcessorProvider<P, R> {

	static val LOGGER = LoggerFactory.getLogger(AbstractSadlProcessorProvider);
	static val CONFIGURATION_ELEMENT_NAME = 'class';

	val Injector injector;
	val Class<P> processorClass;
	val Map<Class<? extends P>, Provider<P>> manuallyAddedProcessors;

	/**
	 * Creates a new provider instance with the class of the processors this
	 * instance provides. This class lazily discovers all service
	 * implementations.
	 */
	protected new(Class<P> processorClass, Injector injector) {
		this.processorClass = processorClass;
		this.injector = injector;
		manuallyAddedProcessors = Maps.newHashMap();
	}

	/**
	 * Returns with the processor for the given subject argument.
	 */
	def P getProcessor(R subject);

	/**
	 * Returns with a view of all available processor instances.
	 */
	def Iterable<P> getAllProcessors() {
		val builder = if (EMFPlugin.IS_ECLIPSE_RUNNING) {
				loadFromExtensionPoints(processorClass)
			} else {
				loadFromSpi(processorClass);
			}
		builder.putAll(manuallyAddedProcessors);
		return ImmutableSet.copyOf(builder.build.values.map[get]);
	}

	/**
	 * Registers a new processor into the underlying cache. If already
	 * registered processor instance will be discarded and the argument will be
	 * registered instead.
	 * 
	 * @param processorProvider
	 *            the provider that provides the new processor instances. Cannot be {@code null}.
	 */
	def registerProcessor(Provider<?> processorProvider) {
		Preconditions.checkNotNull(processorProvider, 'processorProvider');
		val newProcessor = processorProvider.get;
		val key = newProcessor.class;
		val oldProcessor = manuallyAddedProcessors.put(key as Class<? extends P>, processorProvider as Provider<P>);
		if (oldProcessor === null) {
			LOGGER.info('''Processor has been successfully registered into the cache with class: '«key»'.''');
		} else {
			LOGGER.
				info('''Processor has been updated for class: '«key»'. New implementation is '«newProcessor»'. Old implementation was '«oldProcessor»'.''')
		}
	}

	/**
	 * Loads the processors from the Eclipse-based extension points and returns with a builder of processors 
	 * class and processor provider pair.
	 */
	protected def ImmutableMap.Builder<Class<? extends P>, Provider<P>> loadFromExtensionPoints(Class<P> clazz) {
		val builder = ImmutableMap.<Class<? extends P>, Provider<P>>builder;
		if (extensionPointId.present) {
			val registry = RegistryFactory.getRegistry();
			val processors = Suppliers.memoize [
				registry.getConfigurationElementsFor(extensionPointId.get).map [ element |
					val Provider<P> provider = [
						val instance = element.createExecutableExtension(classPropertyName);
						val processor = processorClass.cast(instance);
						// XXX: why not use AbstractGuiceAwareExecutableExtensionFactory instead?
						injector.injectMembers(processor);
						return processor;
					];
					return provider;
				]
			];
			processors.get.forEach [ provider |
				val key = provider.get.class as Class<? extends P>;
				if (!manuallyAddedProcessors.containsKey(key)) {
					builder.put(key, provider);
				}
			];
		}
		return builder;
	}

	/**
	 * Loads the processors using the SPI service discovery approach.
	 */
	protected def ImmutableMap.Builder<Class<? extends P>, Provider<P>> loadFromSpi(Class<P> clazz) {
		val builder = ImmutableMap.<Class<? extends P>, Provider<P>>builder;
		val services = ServiceLoader.load(processorClass).iterator.toList;

		services.forEach [ instance |
			injector.injectMembers(instance);
			val Provider<P> provider = [instance];
			val key = instance.class as Class<? extends P>;
			if (!manuallyAddedProcessors.containsKey(key)) {
				builder.put(key, provider);
			}
		];
		return builder;
	}

	/**
	 * Returns with the underlying injector.
	 */
	def protected getInjector() {
		return injector;
	}

	/**
	 * Returns with the unique Eclipse-based extension point identifier of the processor.
	 * If absent no extension point based discovery will be performed.
	 */
	def protected getExtensionPointId() {
		return Optional.<String>absent;
	}

	/**
	 * Returns with the property name that has to be sued to create the executable
	 * extension from the configuration element of the extension point.
	 * 
	 * <p>
	 * By default it returns with the {@code class} string. Clients may change it.
	 */
	protected def String getClassPropertyName() {
		return CONFIGURATION_ELEMENT_NAME;
	}

}
