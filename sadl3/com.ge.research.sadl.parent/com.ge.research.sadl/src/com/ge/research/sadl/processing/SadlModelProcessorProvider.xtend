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
import com.google.common.collect.MutableClassToInstanceMap
import com.google.inject.Inject
import com.google.inject.Singleton
import java.util.Map
import java.util.ServiceLoader
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.xtext.util.OnChangeEvictingCache
import org.slf4j.LoggerFactory

import static com.google.common.collect.Maps.newHashMap

@Singleton
class SadlModelProcessorProvider implements IModelProcessorProvider {

	static val LOGGER = LoggerFactory.getLogger(SadlModelProcessorProvider);
	static val MODEL_PROCESSOR_CACHE_KEY = 'modelprocessor';

	val Supplier<Map<Class<? extends IModelProcessor>, IModelProcessor>> processors;

	@Inject
	OnChangeEvictingCache cache

	/**
	 * Sole constructor.
	 * <p>
	 * Discovers and caches all available {@link IModelProcessor model processor} instances.
	 */
	@Inject
	new() {
		processors = Suppliers.memoize [
			val builder = ImmutableMap.builder
			ServiceLoader.load(IModelProcessor).iterator.forEach[builder.put(class, it)];
			return MutableClassToInstanceMap.create(newHashMap(builder.build));
		];
	}

	@Override
	override IModelProcessor getProcessor(Resource resource) {
		return cache.get(MODEL_PROCESSOR_CACHE_KEY, resource) [
			doCreateProcessor(resource)
		];
	}

	/**
	 * Creates a model processor instance for the given resource argument.
	 * <p>
	 * By default it returns with a composite model processor instance wrapping
	 * all available model processor instances. Clients may override to add
	 * specific behavior.
	 */
	protected def IModelProcessor doCreateProcessor(Resource resource) {
		return new CompositeModelProcessor(allModelProcessors);
	}

	/**
	 * Returns with a view of all available mode processor instances.
	 */
	def Iterable<IModelProcessor> getAllModelProcessors() {
		ImmutableSet.copyOf(processors.get.values);
	}

	/**
	 * Registers a new processor into the underlying cache. If already
	 * registered processor instance will be discarded and the argument will be
	 * registered instead.
	 * 
	 * @param processor
	 *            the new processor to register. Cannot be {@code null}.
	 */
	def registerModelProcessor(IModelProcessor processor) {
		Preconditions.checkNotNull(processor, 'processor');
		val key = processor.class;
		val oldProcessor = processors.get.put(key, processor);
		if (oldProcessor === null) {
			LOGGER.info('''Model processor has been successfully registered into the cache with class: '«key»'.''');
		} else {
			LOGGER.
				info('''Model processor has been updated for class: '«key»'. New implementation is '«processor»'. Old implementation was '«oldProcessor»'.''')
		}
	}

}
