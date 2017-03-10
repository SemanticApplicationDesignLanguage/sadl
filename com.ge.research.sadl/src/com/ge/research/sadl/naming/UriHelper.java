package com.ge.research.sadl.naming;

import java.util.List;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.naming.QualifiedName;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ge.research.sadl.builder.ResourceManager;

public class UriHelper {
	private static final Logger logger = LoggerFactory
			.getLogger(UriHelper.class);

	public static String parse(QualifiedName name) {
		String importUri = null;
		String nameStr = name.getLastSegment();
		if(nameStr != null) {
			String match1 = SadlSimpleNameProvider.MATCH_TOKEN1;
			int match1Size = match1.length();
			int index1 = nameStr.indexOf(match1);
			if(index1 >= 0) {
				index1 += match1Size; 
				int index2 = nameStr.indexOf(SadlSimpleNameProvider.MATCH_TOKEN2);
				if(index2 > index1) {
					importUri = nameStr.substring(index1, index2);
				}
			}
		}
		return importUri;
	}
	
	public static Resource findMatchingResource(List<Resource> sadlResources,
			String impUri) {
		if (impUri != null) {
			int lastSlash = impUri.lastIndexOf('/');
			String sadlFilename = lastSlash >= 0 ? impUri.substring(lastSlash)
					: impUri;
			if (sadlFilename.endsWith(ResourceManager.getOwlFileExtensionWithPrefix())) {  // ".owl")) {
				sadlFilename = sadlFilename.substring(0,
						sadlFilename.length() - 3) + "sadl";
			}
			for (Resource resource : sadlResources) {
				if (resource.getURI().toString().endsWith(sadlFilename)) {
					logger.debug("Found resouce ("
							+ resource.getURI().toString() + ") matching '"
							+ impUri + "'");
					return resource;
				}
			}
		}
		return null;
	}
}
