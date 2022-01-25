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
package com.ge.research.sadl.processing

import com.google.inject.ImplementedBy
import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.util.ServiceLoader
import org.eclipse.core.runtime.Platform

import static com.ge.research.sadl.processing.ISadlImplicitModelFragmentProvider.*
import static com.google.common.base.Preconditions.*
import static java.lang.System.lineSeparator

import static extension com.google.common.base.Strings.repeat
import static extension org.eclipse.xtext.util.Files.writeStringIntoFile
import org.eclipse.emf.common.EMFPlugin
import org.eclipse.core.runtime.CoreException

/**
 * Representation of an implicit model content provider for the {@code SADL} language.
 * 
 * @author akos.kitta
 */
@ImplementedBy(ISadlImplicitModelContentProvider.Default)
interface ISadlImplicitModelContentProvider {

	/**
	 * Returns with the content of the implicit model that has to be created on demand.
	 */
	def String getContent();

	/**
	 * Writes the SADL implicit model {@link #getContent() content} into the file
	 * under the given resource path. If a resource with the given path exists, then
	 * the existing file will be deleted and a new will be created instead.
	 * 
	 * @param desiredPath
	 *            the path to the desired implicit model resource.
	 * @param setReadOnly
	 *            when {@code true} then the underlying code tries to set write
	 *            protection on the file by calling
	 *            {@link File#setReadOnly()} on the file.
	 * @return the argument.
	 */
	def Path createImplicitModel(Path desiredPath, boolean setReadOnly) {
		checkNotNull(desiredPath, 'desiredPath');
		val file = desiredPath.toFile;
		if (file.exists) {
			Files.delete(desiredPath);
		}

		val parent = desiredPath.parent;
		checkNotNull(parent, '''Missing parent folder for «desiredPath».''');
		if (!parent.toFile.exists) {
			Files.createDirectories(parent);
		}

		if (!file.exists) {
			Files.createFile(desiredPath).toString.writeStringIntoFile(content);
		}

		if (setReadOnly) {
			file.setReadOnly;
		}

		return desiredPath;
	}

	/**
	 * Sugar for {@link #createImplicitModel(Path, boolean)}.
	 */
	def File createImplicitModel(File desiredFile, boolean setReadOnly) {
		return createImplicitModel(checkNotNull(desiredFile, 'desiredFile').toPath, setReadOnly).toFile;
	}

	/**
	 * The default SADL implicit model content provider. 
	 */
	static class Default implements ISadlImplicitModelContentProvider {

		/**
		 * The base content of the SADL implicit model.
		 */
		public static val DEFAULT_CONTENT = '''
			uri "http://sadl.org/sadlimplicitmodel" alias sadlimplicitmodel.
			/****** The content of this model is automatically included in every SADL model ******/
			
			impliedProperty is a type of annotation.
			expandedProperty is a type of annotation.
			ScientificConcept is a class.
			UnittedQuantity is a type of ScientificConcept,
				described by ^value with values of type decimal,
				described by stddev with values of type decimal,
				described by unit with values of type string.
			
			DataDescriptor is a class, described by localDescriptorName (note "If this DataDescriptor is associated with a named parameter, this is the name") with values of type string,
				described by dataType (note "the simple data type, e.g., float") with values of type anyURI,
				described by variableNumArguments (note "true if this DataDescriptor represents one or more value else not present in model") with values of type boolean,
				described by specifiedUnits (note "the array of possible units") with values of type string List,
				described by augmentedType (note "ties the DataDescriptor to the semantic domain model") with values of type AugmentedType,
				described by descriptorVariable (note "This identifies the GPVariable, if any, in the AugmentedType which is associated with this DataDescriptor").
			localDescriptorName of DataDescriptor has at most 1 value.
			dataType of DataDescriptor has at most 1 value.
			specifiedUnits of DataDescriptor has at most 1 value.
			descriptorVariable of DataDescriptor has at most 1 value.
			
			Language is a class.
			{Java, Python, Text, OtherLanguage} are instances of Language.
			Script is a class, described by language with a single value of type Language,
				described by script with a single value of type string.
			^Equation is a class,
				described by expression with values of type Script.
			arguments describes ^Equation with values of type DataDescriptor List.
			arguments of ^Equation has at most 1 value.
			returnTypes describes ^Equation with values of type DataDescriptor List.
			returnTypes of ^Equation has at most 1 value.
			
			ExternalEquation is a type of ^Equation,
				described by externalURI with a single value of type anyURI,
				described by externalURL with values of type anyURI.
				
			AugmentedType is a class.
«««			augTypeUnits describes AugmentedType with a single value of type string List.
			SemanticType (note "allows direct specification of the semantic type of an argument") is a type of AugmentedType, 
				described by semType with a single value of type class.
			GraphPattern is a class.
			{TriplePattern, FunctionPattern} are types of GraphPattern.
			gpSubject describes TriplePattern.
			gpPredicate describes TriplePattern.
			gpObject describes TriplePattern.
			builtin describes FunctionPattern with a single value of type ^Equation.
			GPAtom is a class.
			{GPVariable, GPLiteralValue, GPResource} are types of GPAtom.
			gpVariableName describes GPVariable with a single value of type string.
			gpLiteralValue describes GPLiteralValue with values of type data.
			argValues (note "values of arguments to the built-in") describes FunctionPattern with a single value of type GPAtom List.
			
			SemanticConstraint (note "used to identify necessary patterns in semantic domain terms") is a type of AugmentedType, 
				described by constraints with a single value of type GraphPattern List.
			ThisArgument (note "allows reference to self within an Argument's constraints") is a DataDescriptor.
			
			anyDataType (note "union of all relevant data types") is a type of {decimal or boolean or string or date or dateTime or anyURI}.
			DataTableRow is a class,
				described by rowValues with a single value of type anyDataType List.
			DataTable is a class,
				described by columnDescriptors with a single value of type DataDescriptor List,
				described by dataContent with a single value of type DataTableRow List,
				described by dataLocation with a single value of type anyURI.
			
			^Rule is a class.
			NamedQuery is a class.
			derivedFrom (note "for use with named structures (Equations, NamedQueries, ...), where it has special SADL syntax") 
				is a type of annotation.
			Plus is a GPAtom.
		''';

		override getContent() {
			return fragmentProviders.fold(new StringBuilder(DEFAULT_CONTENT), [ sb, provider |
				sb.append(lineSeparator).append(lineSeparator).append(provider.fragmentToAppend)
			]).toString;
		}

		private def Iterable<ISadlImplicitModelFragmentProvider> getFragmentProviders() {
			return if (EMFPlugin.IS_ECLIPSE_RUNNING) {
				Platform.extensionRegistry.getConfigurationElementsFor(EXTENSION_POINT_ID).map [
					try {
						return createExecutableExtension(EXECUTABLE_ATTRIBUTE_ID) as ISadlImplicitModelFragmentProvider
					} catch (CoreException e) {
						// https://github.com/crapo/sadlos2/commit/58c597e79283c86569d37399e277a3d063ea2423#r30636182
						return null;
					}
				].filterNull;
			} else
				{
					ServiceLoader.load(ISadlImplicitModelFragmentProvider).iterator.toIterable;
				}.map[afterCreation; it];
		}

	}

}
