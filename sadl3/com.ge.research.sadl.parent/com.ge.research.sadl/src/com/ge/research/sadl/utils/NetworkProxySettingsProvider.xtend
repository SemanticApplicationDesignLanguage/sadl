/************************************************************************
 * Copyright 2007-2017 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.utils

import com.google.common.collect.ImmutableMap
import com.google.inject.Singleton
import java.util.Map
import org.eclipse.core.internal.net.ProxyData
import org.eclipse.core.internal.net.ProxySelector

/**
 * Service for providing the network proxy configurations.
 * 
 * @author akos.kitta
 */
@Singleton
class NetworkProxySettingsProvider {

	/**
	 * Returns with a map of network proxy settings. Such as the port and host for
	 * the HTTP and HTTPS proxies.
	 */
	// TODO this really should not be in SADL but in a generic Eclipse core bundle.
	def Map<String, String> getConfigurations() {
		val builder = ImmutableMap.builder;
		// Manual / Direct / Native
		val provider = ProxySelector.defaultProvider;
		ProxySelector.getProxyData(provider).filter[isValid].forEach[
			val type = type.toLowerCase;
			builder.put('''«type».proxyHost''', host);
			builder.put('''«type».proxyPort''', port.toString);	
		];
		return builder.build;
	}
	
	private def isValid(ProxyData it) {
		return port >= 0 && !host.nullOrEmpty && !type.nullOrEmpty;
	}

}
