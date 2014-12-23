package com.ge.research.sadl.ui.editor;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.LinkedList;
import java.util.List;

import javax.inject.Inject;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.emf.common.util.URI;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.hyperlink.IHyperlink;
import org.eclipse.jface.text.hyperlink.IHyperlinkDetector;
import org.eclipse.jface.text.hyperlink.URLHyperlink;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditorPreferenceConstants;
import org.eclipse.xtext.ui.editor.XtextSourceViewerConfiguration;
import org.eclipse.xtext.ui.editor.hyperlinking.XtextHyperlink;

import com.ge.research.sadl.builder.IConfigurationManagerForIDE;
import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.reasoner.ConfigurationException;
import com.ge.research.sadl.ui.SadlActivatorExt;
import com.google.inject.Provider;

public class SadlSourceViewerConfiguration extends
		XtextSourceViewerConfiguration {

	// copied from org.eclipse.ui.internal.editors.text.URLHyperlink
	final class URLHyperlinkExt extends org.eclipse.jface.text.hyperlink.URLHyperlink {
		/**
		 * Creates a new URL hyperlink.
		 *
		 * @param region the region
		 * @param urlString the URL string
		 */
		public URLHyperlinkExt(IRegion region, String urlString) {
			super(region, urlString);
		}

		public void open() {
			// Create the browser
			IWorkbenchBrowserSupport support= PlatformUI.getWorkbench().getBrowserSupport();
			IWebBrowser browser;
			try {
				browser= support.createBrowser(null);
			} catch (PartInitException e) {
				SadlActivatorExt.getInstance().getLog().log(new Status(IStatus.ERROR, SadlActivatorExt.getInstance().getBundle().getSymbolicName(), "Could not create Web browser for URLHyperlink", e)); //$NON-NLS-1$
				super.open();
				return;
			}

			try {
				browser.openURL(new URL(getURLString()));
			} catch (PartInitException e) {
				super.open();
			} catch (MalformedURLException e) {
				super.open();
			}
		}
	}

	@Inject
	private IHyperlinkDetector detector;
	@Inject
	private Provider<XtextHyperlink> hyperlinkProvider;
	@Inject
	private SadlModelManager visitor;

	@Override
	public IHyperlinkDetector[] getHyperlinkDetectors(ISourceViewer sourceViewer) {
		List<IHyperlinkDetector> detectors = new LinkedList<IHyperlinkDetector>();
		IHyperlinkDetector[] inheritedDetectors = getInheritedDetectors(sourceViewer);

		if (inheritedDetectors != null) {
			for (final IHyperlinkDetector detector : inheritedDetectors) {
				detectors.add(new IHyperlinkDetector() {
					public IHyperlink[] detectHyperlinks(ITextViewer textViewer, IRegion region,
							boolean canShowMultipleHyperlinks) {
						try {
							IHyperlink[] hyperlinks = detector.detectHyperlinks(textViewer, region, canShowMultipleHyperlinks);
							mapURLHyperlinks(hyperlinks);
							return hyperlinks;
						}
						catch (Throwable e) {
							// fail safe hyperlink detector - prevent others
							// from failing
						}
						return null;
					}

				});
			}
		}
		detectors.add(detector);
		return detectors.toArray(new IHyperlinkDetector[detectors.size()]);
	}
	
	/**
	 * Map public URLs to Alt URLs. Replaces {@link URLHyperlink}s computed by default by instances with the alternative URL.
	 * @param hyperlinks
	 */
	private void mapURLHyperlinks (IHyperlink[] hyperlinks) {
		if (hyperlinks == null) return;
		
		for (int i=0; i<hyperlinks.length; i++) {
			IHyperlink hyperlink = hyperlinks[i];
			if (hyperlink instanceof URLHyperlink) {
				URLHyperlink urlHyperlink = (URLHyperlink) hyperlink;
				String publicUri = urlHyperlink.getURLString();
				IConfigurationManagerForIDE cmgr = null;
				try {
					// get the configuration manager for the edited resource
					IResource editedResource = (IResource) getEditor().getEditorInput().getAdapter(IResource.class);
					cmgr = visitor.getConfigurationMgr(editedResource.getLocation().toString());
					
					// map the public URL to the mapped URL
					String altUrl = cmgr.getAltUrlFromPublicUri(publicUri);
					XtextHyperlink xtextHyperlink = hyperlinkProvider.get();
					xtextHyperlink.setHyperlinkRegion((Region) hyperlink.getHyperlinkRegion());
					xtextHyperlink.setHyperlinkText(altUrl);
					
					URI uri = URI.createURI(altUrl);
					// TODO: Map File URI to platform URI
					xtextHyperlink.setURI(uri);
					
					hyperlinks[i] = xtextHyperlink;
				} catch (ConfigurationException | URISyntaxException
						| IOException e) {
				}
			}
		}
		
	}
	
	// 
	private IHyperlinkDetector[] getInheritedDetectors (ISourceViewer sourceViewer) {
		if (sourceViewer == null || fPreferenceStore == null)
			return super.getHyperlinkDetectors(sourceViewer);

		if (!fPreferenceStore.getBoolean(AbstractDecoratedTextEditorPreferenceConstants.EDITOR_HYPERLINKS_ENABLED))
			return null;

		return getRegisteredHyperlinkDetectors(sourceViewer);
	}
}
