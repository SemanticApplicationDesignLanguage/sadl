package com.ge.research.sadl.builder;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import com.ge.research.sadl.reasoner.utils.SadlUtils;

public class MessageManager {
	public static enum MessageType { ERROR, WARN, INFO }

	public class HyperlinkInfo {
		private URI linkFile;
		private int fileLineNumber;
		private int fileOffset;
		private int fileLength;
		private int offsetInLink;
		private int linkLength;
		
		public HyperlinkInfo(String fname, int lineno, int foffset, int flength) {
			this(fname, lineno, foffset, flength, -1, -1);
		}
		
		public HyperlinkInfo(String fname, int lineno, int foffset, int flength, int offsetInLnk, int lnkLength) {
			URI location;
			try {
				SadlUtils su = new SadlUtils();
				location = new URI(su.fileNameToFileUrl(fname));
				linkFile = location;
			} catch (URISyntaxException e) {
				linkFile = null;
				e.printStackTrace();
			}
			finally {
				fileLineNumber = lineno;
				fileOffset = foffset;
				fileLength = flength;
				offsetInLink = offsetInLnk;
				linkLength = lnkLength;
			}
		}
		
		public URI getFile() {
			return linkFile;
		}
		
		public int getFileLineNumber() {
			return fileLineNumber;
		}
		
		public int getFileOffset() {
			return fileOffset;
		}
		
		public int getFileLength() {
			return fileLength;
		}

		public int getOffsetInLink() {
			return offsetInLink;
		}

		public int getLinkLength() {
			return linkLength;
		}
	}

	public class SadlMessage {
		private MessageType mtype;
		private String message;
		private HyperlinkInfo linkInfo;
		
		public SadlMessage(MessageType type, String msg, HyperlinkInfo lnkinfo) {
			mtype = type;
			message = msg;
			linkInfo = lnkinfo;
		}
		
		public MessageType getType() {
			return mtype;
		}
		
		public String getMessage() {
			return message;
		}
		
		public HyperlinkInfo getLinkInfo() {
			return linkInfo;
		}
	}
	
	private List<SadlMessage> messages = new ArrayList<SadlMessage>();
	
	private void addMessage(MessageType type, String msg) {
		addMessage(type, msg, null);
	}
	private void addMessage(MessageType type, String msg, HyperlinkInfo lnkinfo) {
		messages.add(new SadlMessage(type, msg, lnkinfo));
	}
	
	public void info(String msg) {
		addMessage(MessageType.INFO, msg);
	}

	public void info(String msg, HyperlinkInfo hyperlinkInfo) {
		addMessage(MessageType.INFO, msg, hyperlinkInfo);
	}

	public void warn(String msg) {
		addMessage(MessageType.WARN, msg);
	}

	public void warn(String msg, HyperlinkInfo hyperlinkInfo) {
        addMessage(MessageType.WARN, msg, hyperlinkInfo);
	}

	public void error(String msg) {
		addMessage(MessageType.ERROR, msg);
	}

	public void error(String msg, HyperlinkInfo hyperlinkInfo) {
        addMessage(MessageType.ERROR, msg, hyperlinkInfo);
	}
	
	public SadlMessage getNextMessage() {
		if (messages.size() > 0) {
			return messages.remove(0);
		}
		return null;
	}
}
