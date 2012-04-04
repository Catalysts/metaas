package uk.co.badgersinfoil.metaas;

import uk.co.badgersinfoil.metaas.dom.ASBinaryExpression;
import uk.co.badgersinfoil.metaas.dom.Expression;
import junit.framework.TestCase;

public class ParentheticExpressionTests extends TestCase {
	private ActionScriptFactory fact = new ActionScriptFactory();

	public void testBasic() {
		// pathenthesised expressions control evaluation order, but
		// don't have any other meaning, therefore metaas doesn't
		// expose them in the DOM, except in their implied effect on
		// the expression tree structure.
		Expression expr = fact.newExpression("(1+1)");
		assertTrue(expr instanceof ASBinaryExpression);
		assertEquals(ASBinaryExpression.Op.ADD, ((ASBinaryExpression)expr).getOperator());
	}
}
