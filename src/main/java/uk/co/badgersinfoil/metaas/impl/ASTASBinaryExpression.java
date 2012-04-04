/*
 * ASTASBinaryExpression.java
 * 
 * Copyright (c) 2008 David Holroyd
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

import uk.co.badgersinfoil.metaas.dom.ASBinaryExpression;
import uk.co.badgersinfoil.metaas.dom.Expression;
import uk.co.badgersinfoil.metaas.impl.antlr.LinkedListTree;

public class ASTASBinaryExpression extends ASTExpression
                                   implements ASBinaryExpression
{
	public ASTASBinaryExpression(LinkedListTree ast) {
		super(ast);
	}

	public Expression getLeftSubexpression() {
		return ExpressionBuilder.build(ast.getFirstChild());
	}

	public Op getOperator() {
		return BinaryOperatorMap.opFromType(ast.getType());
	}

	public Expression getRightSubexpression() {
		return ExpressionBuilder.build(ast.getLastChild());
	}

	public void setLeftSubexpression(Expression left) {
		setSubexpression(0, left);
	}

	public void setOperator(Op operator) {
		BinaryOperatorMap.initialiseFromOp(operator, ast.getToken());
	}

	public void setRightSubexpression(Expression right) {
		setSubexpression(1, right);
	}

	private void setSubexpression(int index, Expression subexpression) {
		ASTExpression sub = (ASTExpression)subexpression;
		LinkedListTree subExpr = sub.getAST();
		ASTBuilder.assertNoParent("expression", subExpr);
		// TODO: handle operator precedence issues
		ast.setChildWithTokens(index, subExpr);
	}
}