package com.ge.research.sadl.ide.rename;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map.Entry;
import java.util.concurrent.ExecutionException;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.lsp4j.Location;
import org.eclipse.lsp4j.SymbolInformation;
import org.eclipse.lsp4j.TextEdit;
import org.eclipse.lsp4j.WorkspaceEdit;
import org.eclipse.xtext.findReferences.IReferenceFinder.IResourceAccess;
import org.eclipse.xtext.ide.refactoring.RefactoringIssueAcceptor.Severity;
import org.eclipse.xtext.ide.server.ILanguageServerAccess;
import org.eclipse.xtext.ide.server.ILanguageServerAccess.IndexContext;
import org.eclipse.xtext.ide.server.UriExtensions;
import org.eclipse.xtext.ide.server.rename.ServerRefactoringIssueAcceptor;
import org.eclipse.xtext.ide.server.symbol.DocumentSymbolService;
import org.eclipse.xtext.ide.server.symbol.WorkspaceSymbolService;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.resource.IResourceDescriptions;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.util.Exceptions;
import org.eclipse.xtext.util.concurrent.IUnitOfWork;

import com.ge.research.sadl.sADL.SadlResource;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import com.google.inject.Inject;

@SuppressWarnings("restriction")
public class SadlResourceRenameHelper {

	@Inject
	private DocumentSymbolService documentSymbolService;

	@Inject
	private WorkspaceSymbolService workspaceSymbolService;

	@Inject
	private IDValueConverter valueConverter;

	@Inject
	private UriExtensions uriExtensions;

	public WorkspaceEdit rename(SadlResource toRename, String newName, ServerRefactoringIssueAcceptor issueAcceptor,
			WorkspaceEdit edit, ILanguageServerAccess languageServerAccess, CancelIndicator cancelIndicator)
			throws InterruptedException, ExecutionException {

		return languageServerAccess.doReadIndex(new Function<IndexContext, WorkspaceEdit>() {

			@Override
			public WorkspaceEdit apply(IndexContext indexContext) {

				IResourceDescriptions indexData = indexContext.getIndex();
				XtextResource resource = (XtextResource) toRename.eResource();
				int offset = NodeModelUtils.findActualNodeFor(toRename).getOffset();
				IResourceAccess resourceAccess = new IResourceAccess() {

					@Override
					public <R> R readOnly(URI targetURI, IUnitOfWork<R, ResourceSet> work) {
						try {
							return languageServerAccess.doRead(uriExtensions.toUriString(targetURI), context -> {
								Resource res = context.getResource();
								if (res == null) {
									return null;
								}
								try {
									return work.exec(res.getResourceSet());
								} catch (Exception e) {
									return Exceptions.throwUncheckedException(e);
								}
							}).get();
						} catch (Exception e) {
							return Exceptions.throwUncheckedException(e);
						}
					}

				};

				String escapedName = valueConverter.mustEscape(newName) ? "^" + newName : newName;
				Multimap<String, Location> locationsPerResource = HashMultimap.create();
				List<? extends Location> locations = documentSymbolService.getReferences(resource, offset,
						resourceAccess, indexData, cancelIndicator);
				for (Location location : locations) {
					locationsPerResource.put(location.getUri(), location);
				}

				// Fail if any of the planned edits has a conflict
				List<? extends SymbolInformation> conflicts = workspaceSymbolService
						.getSymbols("", resourceAccess, indexData, cancelIndicator).getLeft().stream()
						.filter(symbol -> escapedName.equals(symbol.getName()))
						.filter(symbol -> locationsPerResource.containsKey(symbol.getLocation().getUri()))
						.collect(Collectors.toList());

				if (!conflicts.isEmpty()) {
					for (SymbolInformation symbol : conflicts) {
						issueAcceptor.add(Severity.FATAL, "Detected name colision.",
								uriExtensions.toUri(symbol.getLocation().getUri()));
					}
				}

				for (Entry<String, Collection<Location>> entry : locationsPerResource.asMap().entrySet()) {
					String uri = entry.getKey();
					List<TextEdit> edits = mapToEdits(entry.getValue(), escapedName);
					edit.getChanges().put(uri, edits);
				}

				return edit;
			}

		}).get();

	}

	private List<TextEdit> mapToEdits(Collection<Location> locations, String newName) {
		List<TextEdit> edits = new ArrayList<>();
		for (Location location : locations) {
			edits.add(new TextEdit(location.getRange(), newName));
		}
		return edits;
	}

	/**
	 * to have a visible `mustEscape` method.
	 */
	static class IDValueConverter extends org.eclipse.xtext.conversion.impl.IDValueConverter {

		@Override
		public boolean mustEscape(String value) {
			return super.mustEscape(value);
		}

	}

}
