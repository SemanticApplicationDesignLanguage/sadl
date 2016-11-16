package io.typefox.lsp.endpoint.nio.file

import io.typefox.lsp.endpoint.FileAcceptor
import org.junit.Before
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder

class FileAcceptorTest {

	@Rule
	public val folder = new TemporaryFolder

	extension ExpectationExtension

	@Before
	def void setUp() {
		this._expectationExtension = new ExpectationExtension(folder.root.toPath)
	}

	@Test
	def void testNotExists() {
		val fileManager = new FileManager

		val path = rootPath.resolve('foo')
		val fileAcceptor = new FileAcceptor(null)
		fileManager.resolve(path, 0, fileAcceptor)
		assertFile('inexistent', fileAcceptor.rootFile)
	}

	@Test
	def void testDirectoryExists() {
		val fileManager = new FileManager

		val path = createDirectory('foo')
		val fileAcceptor = new FileAcceptor(null)
		fileManager.resolve(path, 0, fileAcceptor)
		assertFile('''
			+ foo
		''', fileAcceptor.rootFile)
	}

	@Test
	def void testFileExists() {
		val fileManager = new FileManager

		val path = createFile('Foo.txt')
		val fileAcceptor = new FileAcceptor(null)
		fileManager.resolve(path, 0, fileAcceptor)
		assertFile('''
			Foo.txt
		''', fileAcceptor.rootFile)
	}

	@Test
	def void testUnresolvedDirectory() {
		val fileManager = new FileManager

		val filePath = createFile('foo/Foo.txt')
		createFile('foo/bar/Bar.txt')
		createDirectory('foo/baz')
		val fileAcceptor = new FileAcceptor(null)
		fileManager.resolve(filePath.parent, 0, fileAcceptor)
		assertFile('''
			+ foo
		''', fileAcceptor.rootFile)
	}

	@Test
	def void testDirectoryWithUnresolvedChildren() {
		val fileManager = new FileManager

		val filePath = createFile('foo/Foo.txt')
		createFile('foo/bar/Bar.txt')
		createDirectory('foo/baz')
		val fileAcceptor = new FileAcceptor(null)
		fileManager.resolve(filePath.parent, 1, fileAcceptor)
		assertFile('''
			- foo
				+ bar
				+ baz
				Foo.txt
		''', fileAcceptor.rootFile)
	}

	@Test
	def void testDirectoryWithAllChildrenResolved() {
		val fileManager = new FileManager

		val filePath = createFile('foo/Foo.txt')
		createFile('foo/bar/Bar.txt')
		createDirectory('foo/baz')
		val fileAcceptor = new FileAcceptor(null)
		fileManager.resolve(filePath.parent, Integer.MAX_VALUE, fileAcceptor)
		assertFile('''
			- foo
				- bar
					Bar.txt
				- baz
				Foo.txt
		''', fileAcceptor.rootFile)
	}

}
