/*
 * ASTASParser.java
 * 
 * Copyright (c) 2006 David Holroyd
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

package uk.co.badgersinfoil.metaas.impl;

import java.io.Reader;
import org.antlr.runtime.RecognitionException;
import org.asdt.core.internal.antlr.AS3Parser;
import uk.co.badgersinfoil.metaas.ActionScriptParser;
import uk.co.badgersinfoil.metaas.dom.ASCompilationUnit;
import uk.co.badgersinfoil.metaas.impl.antlr.LinkedListTree;


public class ASTActionScriptParser implements ActionScriptParser {

	public ASCompilationUnit parse(Reader in) {
		AS3Parser parser = ASTUtils.parse(in);
		LinkedListTree cu;
		try {
			cu = AS3FragmentParser.tree(parser.compilationUnit());
		} catch (RecognitionException e) {
			throw ASTUtils.buildSyntaxException(null, parser, e);
		}
		return new AS3ASTCompilationUnit(cu);
	}
}