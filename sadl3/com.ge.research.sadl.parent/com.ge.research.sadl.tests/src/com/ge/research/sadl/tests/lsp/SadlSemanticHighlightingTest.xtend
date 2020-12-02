package com.ge.research.sadl.tests.lsp

import com.ge.research.sadl.ide.editor.coloring.SadlIdeSemanticHighlightingCalculator
import com.google.inject.Inject
import java.util.List
import java.util.UUID
import org.eclipse.lsp4j.ClientCapabilities
import org.eclipse.lsp4j.SemanticHighlightingCapabilities
import org.eclipse.lsp4j.TextDocumentClientCapabilities
import org.eclipse.xtext.ide.server.UriExtensions
import org.junit.Before
import org.junit.Test

import static com.ge.research.sadl.ide.editor.coloring.SadlIdeSemanticHighlightingCalculator.Scopes.*
import static org.junit.Assert.assertEquals
import static org.junit.Assert.assertNotNull
import static org.junit.Assert.assertTrue

import static extension java.lang.reflect.Modifier.*

class SadlSemanticHighlightingTest extends AbstractSadlLanguageServerTest {

	@Inject
	extension UriExtensions;

	List<List<String>> scopes;

	@Before
	def void before() {
		scopes = initialize[
			capabilities = new ClientCapabilities() => [
				textDocument = new TextDocumentClientCapabilities() => [
					semanticHighlightingCapabilities = new SemanticHighlightingCapabilities() => [
						semanticHighlighting = true;
					];
				];
			];
		].capabilities.semanticHighlighting.scopes;
	}

	@Test
	def void checkStylesAndScopes() {
		val scopes = SadlIdeSemanticHighlightingCalculator.Scopes.declaredFields.filter [
			modifiers.static && modifiers.public && type === List
		];
		val styles = SadlIdeSemanticHighlightingCalculator.Styles.declaredFields.filter [
			modifiers.static && modifiers.public && type === String
		];
		assertEquals(scopes.size, styles.size);
		assertEquals(SadlIdeSemanticHighlightingCalculator.STYLE_MAPPINGS.size, styles.size);
		scopes.forEach [ scope |
			val scopeName = scope.name;
			val expectedStyleName = scopeName.replace('_SCOPES', '_STYLE');
			assertTrue('''Cannot find style '«expectedStyleName»' for scope: «scopeName».''', styles.exists [
				name == expectedStyleName
			]);
		];
		styles.forEach [ style |
			val styleName = style.name;
			val expectedScopeName = styleName.replace('_STYLE', '_SCOPES');
			assertTrue('''Cannot find scope '«expectedScopeName»' for style: «styleName».''', scopes.exists [
				name == expectedScopeName
			]);
		];
	}

	@Test
	def void checkColoring_01() {
		'''
		uri "http://sadl.imp/shapes_top".
		
		Shape is a top-level class.'''.assertInfos('''
		0 : []
		1 : []
		2 : [0:5:«CLASS_SCOPES»]''');
	}

	@Test
	def void checkColoring_02() {
		'''
		uri "http://sadl.imp/shapes_specific".
		import "file://shapes-top.sadl" as shapes-top.
		
		Circle is a type of Shape,
			described by radius with values of type float.
		
		Rectangle is a type of Shape,
			described by height with values of type float,
			described by width with values of type float.'''.assertInfos('''
		0 : []
		1 : [7:24:«URI_SCOPES»]
		2 : []
		3 : [0:6:«CLASS_SCOPES», 20:5:«VARIABLE_SCOPES»]
		4 : [14:6:«DATA_PROPERTY_SCOPES»]
		5 : []
		6 : [0:9:«CLASS_SCOPES», 23:5:«VARIABLE_SCOPES»]
		7 : [14:6:«DATA_PROPERTY_SCOPES»]
		8 : [14:5:«DATA_PROPERTY_SCOPES»]''');
	}

	@Test
	def void checkColoring_03() {
		'''
		uri "http://sadl.imp/shapes_test" .
		import "file://shape-rules.sadl" as shape-rules.
		
		MyCircle is a Circle, has radius 3.5 .
		
		MyRect is a Rectangle, has height 3.5, has width 4.5.
		
		Test: MyCircle has area 38.48 .'''.assertInfos('''
		0 : []
		1 : [7:25:«URI_SCOPES»]
		2 : []
		3 : [0:8:«INSTANCE_SCOPES», 14:6:«VARIABLE_SCOPES», 26:6:«VARIABLE_SCOPES»]
		4 : []
		5 : [0:6:«INSTANCE_SCOPES», 12:9:«VARIABLE_SCOPES», 27:6:«VARIABLE_SCOPES», 43:5:«VARIABLE_SCOPES»]
		6 : []
		7 : [6:8:«INSTANCE_SCOPES», 19:4:«VARIABLE_SCOPES»]''');
	}

	protected def String open(CharSequence content) {
		return open(content, UUID.randomUUID.toString);
	}

	protected def String open(CharSequence content, String fileName) {
		val file = root.toPath.resolve('''«fileName».«fileExtension»''').toFile;
		val uri = file.toURI.toUriString;
		uri.open(content.toString);
		return uri;
	}

	protected def void assertInfos(CharSequence content, String expected) {
		val uri = open(content, 'MyModel');
		val params = semanticHighlightingParams;
		assertEquals(1, params.size);
		val entry = params.entrySet.findFirst[key.uri == uri];
		assertNotNull(entry);
		val actual = entry.value.map[it -> scopes].map[toExpectation].join('\n');
		assertEquals(expected, actual);
	}
}
