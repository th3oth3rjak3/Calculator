namespace Calculator;

public partial class App : Application
{
    public App()
    {
        InitializeComponent();

        MainPage = new MainPage();

    }

    protected override Window CreateWindow(IActivationState activationState)
    {
        var window = base.CreateWindow(activationState);
        var height = 600;
        var width = 400;

        window.Width = width;
        window.Height = height;

        window.MinimumHeight = height;
        window.MinimumWidth = width;

        window.MaximumHeight = height;
        window.MaximumWidth = width;

        return window;
    }
}
