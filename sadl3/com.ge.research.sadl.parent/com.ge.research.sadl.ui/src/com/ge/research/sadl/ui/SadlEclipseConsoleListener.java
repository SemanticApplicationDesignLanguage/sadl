package com.ge.research.sadl.ui;

import java.util.HashMap;
import java.util.Iterator;

import org.eclipse.core.internal.localstore.FileSystemResourceManager;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.debug.ui.console.FileLink;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.FindReplaceDocumentAdapter;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.jface.text.IRegion;
import org.eclipse.ui.console.MessageConsole;

import com.ge.research.sadl.builder.MessageManager.HyperlinkInfo;

public class SadlEclipseConsoleListener implements IDocumentListener {
	private MessageConsole mc;
	private HashMap<String, HyperlinkInfo> hyperlinks;
	private int startingOffset;

	public SadlEclipseConsoleListener(HashMap<String, HyperlinkInfo> links, int start) {
		mc = SadlEclipseConsole.findOrCreateConsole();
		hyperlinks = links;
		startingOffset = start;
	}
	
	@Override
	public void documentChanged(DocumentEvent event) {
		if (hyperlinks != null) {
			try {
				IDocument doc = event.getDocument();
				Iterator<String> itr = hyperlinks.keySet().iterator();
				int lastOffset = startingOffset;
				while (itr.hasNext()) {
					String txt = itr.next();
					FindReplaceDocumentAdapter frda = new FindReplaceDocumentAdapter(doc);
					IRegion region = frda.find(lastOffset, txt, true, true, false, false);
					if (region != null) {
						HyperlinkInfo info = hyperlinks.get(txt);
						int linkOffset = region.getOffset();
						int linkLength = region.getLength();
						if (info.getOffsetInLink() > 0) {
							linkOffset += info.getOffsetInLink();
							linkLength -= info.getOffsetInLink();
						}
						if (info.getLinkLength() > 0) {
							linkLength = info.getLinkLength();
						}
						if (linkOffset >= 0 && linkLength > 0 && info.getFileOffset() >= 0 && info.getFileLength() > 0) {
							IPath path = new Path(info.getLinkResource().toString());
							IFile file = null; //project.getFile(path);
//							FileLink fileLink = new FileLink(file, null, info.getFileOffset(), info.getFileLength(), info.getFileLineNumber());
//							mc.addHyperlink(fileLink, linkOffset, linkLength);
						}
//						if (linkOffset > lastOffset) {
//							lastOffset = linkOffset;
//						}
					}
				}
			} catch (BadLocationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			finally {
				hyperlinks = null;
			}
		}
	}

	@Override
	public void documentAboutToBeChanged(DocumentEvent event) {
		// TODO Auto-generated method stub
		
	}

}
