using Calculator.Library.Services;

using Functional.Monadic;
using Functional.Options;

using Shouldly;

namespace Calculator.Tests;

public class TestCalculatorLibrary
{
    [Fact]
    public void ItShouldDoBasicAddition() =>
        new CalculatorService()
            .TryCalculate("1 + 1")
            .Map(value => value.Tap(_ => value.ShouldBe(2d)))
            .Reduce(() => throw new ShouldAssertException("It should have been Some"));

    [Fact]
    public void ItShouldDoBasicSubtraction() =>
        new CalculatorService()
            .TryCalculate("4 - 1")
            .Map(value => value.Tap(_ => value.ShouldBe(3d)))
            .Reduce(() => throw new ShouldAssertException("It should have been Some"));

    [Fact]
    public void ItShouldDoBasicMultiplication() =>
        new CalculatorService()
            .TryCalculate("3 * 4")
            .Map(value => value.Tap(_ => value.ShouldBe(12d)))
            .Reduce(() => throw new ShouldAssertException("It should have been Some"));

    [Fact]
    public void ItShouldDoBasicDivision() =>
        new CalculatorService()
            .TryCalculate("10 / 2")
            .Map(value => value.Tap(_ => value.ShouldBe(5d)))
            .Reduce(() => throw new ShouldAssertException("It should have been Some"));

    [Fact]
    public void ItShouldHandleDivByZero() =>
    new CalculatorService()
        .TryCalculate("1 / 0")
            .Match(
                some => throw new ShouldAssertException("It should have been None."),
                () => "It was None :)");

    [Fact]
    public void ItShouldHandleNegativeAddition() =>
        new CalculatorService()
            .TryCalculate("-1 + -1")
            .Map(value => value.Tap(_ => value.ShouldBe(-2d)))
            .Reduce(() => throw new ShouldAssertException("It should have been Some"));

    [Fact]
    public void ItShouldHandleNegativeSubtraction() =>
        new CalculatorService()
            .TryCalculate("-1 - -1")
            .Map(value => value.Tap(_ => value.ShouldBe(0d)))
            .Reduce(() => throw new ShouldAssertException("It should have been Some"));

    [Fact]
    public void ItShouldHandleNegativeMultiplication() =>
        new CalculatorService()
            .TryCalculate("-2 * -2")
            .Map(value => value.Tap(_ => value.ShouldBe(4d)))
            .Reduce(() => throw new ShouldAssertException("It should have been Some"));

    [Fact]
    public void ItShouldHandleNegativeDivision() =>
        new CalculatorService()
            .TryCalculate("-6 / -3")
            .Map(value => value.Tap(_ => value.ShouldBe(2d)))
            .Reduce(() => throw new ShouldAssertException("It should have been Some"));

    [Fact]
    public void ItShouldHandleDecimalAddition() =>
        new CalculatorService()
            .TryCalculate("1.2 + 5.4")
            .Map(value => value.Tap(_ => value.ShouldBeInRange(6.6d, 6.600000001d)))
            .Reduce(() => throw new ShouldAssertException("It should have been Some"));

    [Fact]
    public void ItShouldHandleDecimalSubtraction() =>
        new CalculatorService()
            .TryCalculate("2.4 - 1.4")
            .Map(value => value.Tap(_ => value.ShouldBe(1.0d)))
            .Reduce(() => throw new ShouldAssertException("It should have been Some"));

    [Fact]
    public void ItShouldHandleDecimalMultiplication() =>
        new CalculatorService()
            .TryCalculate("1.0 * 2.4")
            .Map(value => value.Tap(_ => value.ShouldBe(2.4d)))
            .Reduce(() => throw new ShouldAssertException("It should have been Some"));

    [Fact]
    public void ItShouldHandleDecimalDivision() =>
        new CalculatorService()
            .TryCalculate("2.2 / 1.1")
            .Map(value => value.Tap(_ => value.ShouldBe(2d)))
            .Reduce(() => throw new ShouldAssertException("It should have been Some"));

    [Fact]
    public void ItShouldHandleNegativeDecimalAddition() =>
        new CalculatorService()
            .TryCalculate("-1.2 + 4.5")
            .Map(value => value.Tap(_ => value.ShouldBe(3.3d)))
            .Reduce(() => throw new ShouldAssertException("It should have been Some"));

    [Fact]
    public void ItShouldHandleNegativeDecimalSubtraction() =>
    new CalculatorService()
        .TryCalculate("-1.2 - 4.5")
        .Map(value => value.Tap(_ => value.ShouldBe(-5.7d)))
        .Reduce(() => throw new ShouldAssertException("It should have been Some"));

    [Fact]
    public void ItShouldHandleNegativeDecimalMultiplication() =>
    new CalculatorService()
        .TryCalculate("-1.2 * 4.5")
        .Map(value => value.Tap(_ => value.ShouldBeInRange(-5.400d, -5.399d)))
        .Reduce(() => throw new ShouldAssertException("It should have been Some"));

    [Fact]
    public void ItShouldHandleNegativeDecimalDivision() =>
    new CalculatorService()
        .TryCalculate("-1.2 / 4.5")
        .Map(value => value.Tap(_ => value.ShouldBeInRange(-0.2667d, -0.2666d)))
        .Reduce(() => throw new ShouldAssertException("It should have been Some"));

    [Fact]
    public void ItShouldHandleMultipleOperations() =>
        new CalculatorService()
            .TryCalculate("-1 + 2 / 4 * 45")
            .Map(value => value.Tap(_ => value.ShouldBe(21.5d)))
            .Reduce(() => throw new ShouldAssertException("It should have been Some"));

    [Fact]
    public void ItShouldHandleMultipleOperationsWithParends() =>
        new CalculatorService()
            .TryCalculate("(-1 + 5) / (4 * -2)")
            .Map(value => value.Tap(_ => value.ShouldBe(-0.5d)))
            .Reduce(() => throw new ShouldAssertException("It should have been Some"));

    [Fact]
    public void ItShouldHandleMultipleOperationsWithParends2() =>
    new CalculatorService()
        .TryCalculate("-1 + (5 / 4 * -2)")
        .Map(value => value.Tap(_ => value.ShouldBe(-3.5d)))
        .Reduce(() => throw new ShouldAssertException("It should have been Some"));

    [Fact]
    public void ItShouldHandleNegativeParends() =>
        new CalculatorService()
            .TryCalculate("2 + -(4)")
            .Map(value => value.Tap(_ => value.ShouldBe(-2d)))
            .Reduce(() => throw new ShouldAssertException("It should have been Some"));

    [Fact]
    public void ItShouldHandleNegativeParends1() =>
        new CalculatorService()
            .TryCalculate("2 + -(4 * 2)")
            .Map(value => value.Tap(_ => value.ShouldBe(-6d)))
            .Reduce(() => throw new ShouldAssertException("It should have been Some"));

    [Fact]
    public void ItShouldHandleMalformedExpressions() =>
        new CalculatorService()
            .TryCalculate(")(")
            .Match(
                value => "Some".Tap(_ => throw new ShouldAssertException("It should have been None")),
                () => "None");

    [Fact]
    public void ItShouldHandleMalformedExpressions2() =>
        new CalculatorService()
            .TryCalculate("1 ) 1")
            .Match(
                value => "Some".Tap(_ => throw new ShouldAssertException($"It should have been None but was {value}")),
                () => "None");

    [Fact]
    public void ItShouldHandleMalformedExpressions3() =>
        new CalculatorService()
            .TryCalculate("1  1")
            .Match(
                value => "Some".Tap(_ => throw new ShouldAssertException($"It should have been None but was {value}")),
                () => "None");

    [Fact]
    public void ItShouldHandleMalformedExpressions4() =>
    new CalculatorService()
        .TryCalculate("1  1 -")
        .Match(
            value => "Some".Tap(_ => throw new ShouldAssertException($"It should have been None but was {value}")),
            () => "None");
    [Fact]
    public void ItShouldHandleMalformedExpressions5() =>
        new CalculatorService()
            .TryCalculate("1 + 1 -")
            .Map(value => value.Tap(_ => value.ShouldBe(2d)))
            .Reduce(() => throw new ShouldAssertException("It should have been Some"));
}
