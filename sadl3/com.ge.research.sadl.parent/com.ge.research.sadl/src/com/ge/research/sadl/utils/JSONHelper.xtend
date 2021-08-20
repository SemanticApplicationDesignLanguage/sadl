/************************************************************************
 * Copyright 2007-2018 General Electric Company, All Rights Reserved
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

import com.google.common.base.Preconditions
import com.google.common.io.Files
import com.google.inject.Singleton
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.Map
import javax.script.ScriptEngine
import javax.script.ScriptEngineManager

/**
 * Helper for for parsing JSON files with zero third party dependencies.. 
 */
@Singleton
class JSONHelper {

	static ScriptEngine ENGINE;

	private static def getOrCreateEngine() {
		if (ENGINE === null) {
			synchronized (JSONHelper) {
				if (ENGINE === null) {
					ENGINE = new ScriptEngineManager().getEngineByName('javascript');
				}
			}
		}
		return ENGINE;
	}

	/**
	 * Reads the a JSON file and returns with a map of the property key-values. Throws a runtime exception
	 * if the file does not exist or it is an invalid JSON file.
	 */
	def Map<Object, Object> asMap(Path path) {
		try {
			val json = Files.asCharSource(path.toFile, StandardCharsets.UTF_8).read();
			val result = getOrCreateEngine.eval('''Java.asJSONCompatible(«json»)''');
			if (result === null) {
				return emptyMap;
			}
			Preconditions.checkState(result instanceof Map<?, ?>);
			return result as Map<Object, Object>;
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

}
