/************************************************************************
 * Copyright © 2007-2017 - General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.ui

import com.ge.research.sadl.ui.internal.SadlActivator
import org.eclipse.ui.plugin.AbstractUIPlugin
import com.google.common.cache.CacheBuilder
import com.google.common.cache.LoadingCache
import org.eclipse.jface.resource.ImageDescriptor
import org.eclipse.swt.graphics.Image

/**
 * A very naive image registry for SADL.
 * 
 * @author akos.kitta
 */
class SadlImageRegistry {

	static val PLUGIN_ID = SadlActivator.getInstance().getBundle().getSymbolicName();
	static val ICON_FOLDER = 'icons';
	static val LoadingCache<ImageDescriptor, Image> IMAGE_CACHE = CacheBuilder.newBuilder.build([
		return createImage;
	]);

	/**
	 * Returns with the image for the given file name. The file should exist in the 
	 * {@code /icons} folder in this plug-in. If you need to get a file {@code foo.png}
	 * from the {@code icons/foo.png}, then you should pass simple {@code foo.png}.
	 * 
	 * Images are cached, so do not dispose them. In general do not dispose any SWT 
	 * resources which is not created by you via its constructor. If you have created 
	 * a new instance, then you are responsible to clean it up.
	 * 
	 * Indeed to be able to use this, you need a running workbench. 
	 */
	static def getImage(String fileName) {
		val activator = SadlActivator.instance;
		val registry = activator.imageRegistry;
		var descriptor = registry.getDescriptor(fileName);
		if (descriptor === null) {
			descriptor = AbstractUIPlugin.imageDescriptorFromPlugin(PLUGIN_ID, '''«ICON_FOLDER»/«fileName»''');
			registry.put(fileName, descriptor);
		}
		return IMAGE_CACHE.getUnchecked(descriptor);
	}
	
	static interface Constants { 
		
		val CA_TYPE = 'envvar_obj.gif';
			
	}

}
