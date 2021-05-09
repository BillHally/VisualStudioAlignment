#nullable enable

using System;
using System.Runtime.InteropServices;
using System.Threading;
using Microsoft;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

using Task = System.Threading.Tasks.Task;

namespace Hally.Alignment.VisualStudio
{
    /// <summary>
    /// This is the class that implements the package exposed by this assembly.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The minimum requirement for a class to be considered a valid package for Visual Studio
    /// is to implement the IVsPackage interface and register itself with the shell.
    /// This package uses the helper classes defined inside the Managed Package Framework (MPF)
    /// to do it: it derives from the Package class that provides the implementation of the
    /// IVsPackage interface and uses the registration attributes defined in the framework to
    /// register itself and its components with the shell. These attributes tell the pkgdef creation
    /// utility what data to put into .pkgdef file.
    /// </para>
    /// <para>
    /// To get loaded into VS, the package must be referred by &lt;Asset Type="Microsoft.VisualStudio.VsPackage" ...&gt; in .vsixmanifest file.
    /// </para>
    /// </remarks>
    [PackageRegistration(UseManagedResourcesOnly = true, AllowsBackgroundLoading = true)]
    [Guid(AlignmentVisualStudioPackage.PackageGuidString)]
    [ProvideMenuResource("Menus.ctmenu", 1)]
    [ProvideBindingPath]
    public sealed class AlignmentVisualStudioPackage : AsyncPackage
    {
        public const string PackageGuidString = "5aaf5238-578a-4f2c-a73a-442c41d69752";

        private static readonly Guid OutputPaneGuid = new Guid("16D33126-2DB6-4CF7-9325-52CDD553FC94");
        private const string PaneTitle = "Line alignment";
        private static IVsOutputWindowPane outputPane;

        private static IVsOutputWindowPane CreatePane(IServiceProvider services, Guid paneGuid, string title, bool visible, bool clearWithSolution)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            var outputWindow = services.GetService(typeof(SVsOutputWindow)) as IVsOutputWindow;

            if (outputWindow == null)
            {
                return null;
            }

            outputWindow.CreatePane(
                ref paneGuid,
                title,
                Convert.ToInt32(visible),
                Convert.ToInt32(clearWithSolution));

            outputWindow.GetPane(ref paneGuid, out IVsOutputWindowPane outputPane);

            return outputPane;
        }

        public static void WriteToOutputPane(IServiceProvider services, string message)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            if (AlignmentVisualStudioPackage.outputPane == null)
            {
                AlignmentVisualStudioPackage.outputPane = CreatePane(services, OutputPaneGuid, PaneTitle, true, true);
            }

            AlignmentVisualStudioPackage.outputPane.OutputString(message);
        }
    }
}
