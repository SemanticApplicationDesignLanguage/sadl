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
package com.ge.research.sadl.processing;

import java.util.Collection;

import com.ge.research.sadl.model.gp.SadlCommand;

/**
 * Representation of a inference status. This status represents the outcome of
 * an inference execution and gathers all possible results, errors and the
 * executed {@code SADL} commands.
 * 
 * @author akos.kitta
 *
 */
public interface IInferenceStatus {

	/**
	 * Returns with a collection of commands that were executed during the
	 * inference. Could return with an empty collection but never returns with
	 * {@code null}.
	 * 
	 * @return a collection of executed {@code SADL} command.
	 */
	Collection<SadlCommand> getExecutedCommands();

	/**
	 * Returns with a collection of inference results gathered during the
	 * inference process. Might return with an empty collection but must never
	 * return with {@code null}.
	 * 
	 * @return a collection of inference results. Could be empty, but never
	 *         {@code null}.
	 */
	Collection<IInferenceResult> getResults();

	/**
	 * Returns with a collection of error messages describing all the errors
	 * occurred during the inference process. Might return with an empty
	 * collection, if no errors happened but must never return with
	 * {@code null}.
	 * 
	 * @return a collection of error messages occurred during the inference.
	 */
	Collection<String> getErrorMessages();

	/**
	 * Marker interface of a {@code SADL} inference result.
	 */
	static interface IInferenceResult {

	}

}
