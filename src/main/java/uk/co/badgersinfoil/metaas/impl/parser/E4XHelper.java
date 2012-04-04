/*
 * E4XHelper.java
 * 
 * Copyright (c) 2006-2007 David Holroyd
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.co.badgersinfoil.metaas.impl.parser;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import org.antlr.runtime.ANTLRReaderStream;
import org.antlr.runtime.CharStream;
import org.antlr.runtime.RecognitionException;
import org.asdt.core.internal.antlr.AS3Lexer;
import uk.co.badgersinfoil.metaas.impl.AS3FragmentParser;
import uk.co.badgersinfoil.metaas.impl.antlr.LinkedListToken;
import uk.co.badgersinfoil.metaas.impl.antlr.LinkedListTokenSource;
import uk.co.badgersinfoil.metaas.impl.antlr.LinkedListTokenStream;
import uk.co.badgersinfoil.metaas.impl.antlr.LinkedListTree;
import uk.co.badgersinfoil.metaas.impl.antlr.LinkedListTreeAdaptor;
import uk.co.badgersinfoil.metaas.impl.parser.e4x.E4XParser;
import uk.co.badgersinfoil.metaas.impl.parser.e4x.E4XLexer;


public class E4XHelper {
	private static final LinkedListTreeAdaptor TREE_ADAPTOR = new LinkedListTreeAdaptor();

	/**
	 * Creates a properly-configured parser object for the E4X grammar.
	 */
	public static E4XParser parserOn(Reader in) throws IOException {
		ANTLRReaderStream cs = new ANTLRReaderStream(in);
		E4XLexer lexer = new E4XLexer(cs);
		LinkedListTokenSource source = new LinkedListTokenSource(lexer);
		LinkedListTokenStream stream = new LinkedListTokenStream(source);
		E4XParser parser = new E4XParser(stream);
		parser.setTreeAdaptor(TREE_ADAPTOR);
		parser.setInput(lexer, cs);
		return parser;
	}

	public static LinkedListTree parseXMLLiteral(AS3Lexer lexer, CharStream cs, LinkedListTokenStream stream) throws RecognitionException {
		String tail = cs.substring(cs.index(), cs.size()-1);
		int initialTailLength = tail.length();
		E4XParser parser;
		try {
			parser = e4xParserOn(new StringReader(tail), stream);
		} catch (IOException e) {
			// TODO: better exception type?
			throw new RuntimeException(e);
		}
		LinkedListTree ast = AS3FragmentParser.tree(parser.xmlPrimary());
		tail = parser.getInputTail();
		// skip over the XML in the original, underlying CharStream
		cs.seek(cs.index() + (initialTailLength - tail.length()));
		LinkedListTokenSource source = (LinkedListTokenSource)stream.getTokenSource();
		stream.setTokenSource(source);  // cause any remembered E4X state to be dropped
		stream.scrub(1); // erase the subsequent token that the E4X parser got from this stream
		source.setDelegate(lexer);
		return ast;
	}

	private static E4XParser e4xParserOn(Reader in, LinkedListTokenStream stream) throws IOException {
		ANTLRReaderStream cs = new ANTLRReaderStream(in);
		E4XLexer lexer = new E4XLexer(cs);
		LinkedListTokenSource source = (LinkedListTokenSource)stream.getTokenSource();
		source.setDelegate(lexer);
		
		// The AS3 grammar will see the initial '<' as an LT (less-than)
		// token, and lookahead in the AS3Parser will have already
		// grabbed references to that token in order to make it the
		// startToken for various AST subtrees, so we can't just delete
		// it.  We therefore find the LT token and change its type to
		// match the E4X vocabulary, and then rewind the token stream
		// so that this will be the first token that the E4XParser will
		// see.
		LinkedListToken startMarker = (LinkedListToken)stream.LT(-1);
		startMarker.setType(E4XParser.XML_LCHEVRON);
		stream.seek(stream.index()-1);

		E4XParser parser = new E4XParser(stream);
		parser.setTreeAdaptor(new LinkedListTreeAdaptor());
		parser.setInput(lexer, cs);
		return parser;
	}

}