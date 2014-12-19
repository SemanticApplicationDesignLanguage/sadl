package com.ge.research.sadl.ui.editor;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.LinkedList;
import java.util.List;

import javax.inject.Inject;

import org.eclipse.core.resources.IResource;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.hyperlink.IHyperlink;
import org.eclipse.jface.text.hyperlink.IHyperlinkDetector;
import org.eclipse.jface.text.hyperlink.URLHyperlink;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditorPreferenceConstants;
import org.eclipse.xtext.ui.editor.XtextSourceViewerConfiguration;

import com.ge.research.sadl.builder.ConfigurationManagerForIDE;
import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.reasoner.ConfigurationException;

public class SadlSourceViewerConfiguration extends
		XtextSourceViewerConfiguration {

	@Inject
	private IHyperlinkDetector detector;

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
				ConfigurationManagerForIDE cmgr = null;
				try {
					// get the configuration manager for the edited resource
					IResource editedResource = (IResource) getEditor().getEditorInput().getAdapter(IResource.class);
					cmgr = visitor.getConfigurationMgr(editedResource.getLocation().toString());
					
					// map the public URL to the mapped URL
					String altUrl = cmgr.getAltUrlFromPublicUri(publicUri);
					urlHyperlink = new URLHyperlink(hyperlink.getHyperlinkRegion(), altUrl);
					// replace hyperlink with mapped one
					hyperlinks[i] = urlHyperlink;
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
