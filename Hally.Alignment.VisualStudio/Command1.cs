using System;
using System.ComponentModel.Composition;
using System.ComponentModel.Design;
using System.Globalization;
using System.Threading;
using System.Threading.Tasks;

using EnvDTE;

using Microsoft.VisualStudio.Commanding;

using Microsoft.VisualStudio.Editor.Commanding;

using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Editor.Commanding;
using Microsoft.VisualStudio.Text.Operations;

using Microsoft.VisualStudio.Utilities;

using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

using Task = System.Threading.Tasks.Task;

namespace Hally.Alignment.VisualStudio
{
    public static class AlignLines
    {
        public static void AlignSelectedLines(ITextView textView, IEditorOperations editorOperations)
        {
            var selection = textView.Selection;

            if (!selection.IsActive)
            {
                // TODO: check whether this test is necessary (i.e. if false, is this method ever invoked?)
                return;
            }

            if (selection.IsEmpty)
            {
                // TODO: do something sensible when there's no selection
                return;
            }

            if (selection.Mode == TextSelectionMode.Box)
            {
                // TODO: do something sensible when there's a Box selection
                return;
            }

            var isReversed = selection.IsReversed;
            var selectedSpan = selection.SelectedSpans[0]; // When it's not a Box selection there's only 1 selected span
            var start = selectedSpan.Span.Start;

#if DEBUG
            try
            {
#endif
                // If necessary, extend the selection to the start of the first line of the selection, so that columns
                // of the all the lines in the selection match up
                var textLine = selection.Start.Position.GetContainingLine();
                if (textLine.Start.Position != start)
                {
                    selectedSpan = new SnapshotSpan(selectedSpan.Snapshot, textLine.Start.Position, start - textLine.Start.Position + selectedSpan.Length);
                    start = textLine.Start.Position;
                    textView.Selection.Select(selectedSpan, isReversed);
                }

                // Realign the selected text and replace the current selection with the result
                var text = selectedSpan.GetText();
                var realigned = Alignment.RealignAll(text);
                var updated = textView.TextBuffer.Replace(selectedSpan, realigned);

                // Redo the original selection, expanded or reduced to the extent of the realigned text
                var updatedSelectionSpan = new SnapshotSpan(updated, start, realigned.Length);
                textView.Selection.Select(updatedSelectionSpan, isReversed);

                // If the original selection had the caret at the start, move it back there now (the text replacement leaves the caret at the end,
                // and redoing the selection doesn't affect the caret)
                if (isReversed)
                {
                    textView.Caret.MoveTo(updatedSelectionSpan.Start);
                }
#if DEBUG
            }
            catch (Exception ex) {
                textView.TextBuffer.Insert(selectedSpan.Span.Start, ex.ToString());
            }
#endif
        }
    }

    public class AlignLinesCommandArgs : EditorCommandArgs
    {
        public AlignLinesCommandArgs(ITextView textView, ITextBuffer textBuffer)
            : base(textView, textBuffer)
        {
        }
    }

    public class AlignLinesCommandBinding
    {
        private const int AlignLinesCommandId = 0x0200;

        private const string CommandSetValue = "7404ba69-b772-4931-8783-7d904aefa62c";

        [Export]
        [CommandBinding(CommandSetValue, AlignLinesCommandId, typeof(AlignLinesCommandArgs))]
        internal CommandBindingDefinition alignLinesCommandBinding;
    }

    [Export(typeof(ICommandHandler))]
    [ContentType("text")]
    [Name(nameof(AlignLinesCommandHandler))]
    public class AlignLinesCommandHandler : ICommandHandler<AlignLinesCommandArgs>
    {
        public string DisplayName => "Align Selected Lines";

        [Import]
        private IEditorOperationsFactoryService EditorOperations = null;

        public CommandState GetCommandState(AlignLinesCommandArgs args)
        {
            return args.TextView.Selection.IsEmpty ? CommandState.Unavailable : CommandState.Available;
        }

        public bool ExecuteCommand(AlignLinesCommandArgs args, CommandExecutionContext context)
        {
            using (context.OperationContext.AddScope(allowCancellation: false, description: "Aligning selected lines"))
            {
                AlignLines.AlignSelectedLines(args.TextView, EditorOperations.GetEditorOperations(args.TextView));
            }

            return true;
        }
    }
/*
    /// <summary>
    /// Command handler
    /// </summary>
    internal sealed class Command1
    {
        /// <summary>
        /// Command ID.
        /// </summary>
        public const int CommandId = 0x0100;

        /// <summary>
        /// Command menu group (command set GUID).
        /// </summary>
        private const string CommandSetValue = "7404ba69-b772-4931-8783-7d904aefa62c";
        public static readonly Guid CommandSet = new Guid(CommandSetValue);

        /// <summary>
        /// VS Package that provides this command, not null.
        /// </summary>
        private readonly AsyncPackage package;

        /// <summary>
        /// Initializes a new instance of the <see cref="Command1"/> class.
        /// Adds our command handlers for menu (commands must exist in the command table file)
        /// </summary>
        /// <param name="package">Owner package, not null.</param>
        /// <param name="commandService">Command service to add command to, not null.</param>
        private Command1(AsyncPackage package, OleMenuCommandService commandService)
        {
            this.package = package ?? throw new ArgumentNullException(nameof(package));
            commandService = commandService ?? throw new ArgumentNullException(nameof(commandService));

            var menuCommandID = new CommandID(CommandSet, CommandId);
            var menuItem = new MenuCommand(this.Execute, menuCommandID);
            commandService.AddCommand(menuItem);
        }

        /// <summary>
        /// Gets the instance of the command.
        /// </summary>
        public static Command1 Instance
        {
            get;
            private set;
        }

        /// <summary>
        /// Gets the service provider from the owner package.
        /// </summary>
        private Microsoft.VisualStudio.Shell.IAsyncServiceProvider ServiceProvider
        {
            get
            {
                return this.package;
            }
        }

        /// <summary>
        /// Initializes the singleton instance of the command.
        /// </summary>
        /// <param name="package">Owner package, not null.</param>
        public static async Task InitializeAsync(AsyncPackage package)
        {
            // Switch to the main thread - the call to AddCommand in Command1's constructor requires
            // the UI thread.
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync(package.DisposalToken);

            OleMenuCommandService commandService = await package.GetServiceAsync(typeof(IMenuCommandService)) as OleMenuCommandService;
            Instance = new Command1(package, commandService);
        }

        /// <summary>
        /// This function is the callback used to execute the command when the menu item is clicked.
        /// See the constructor to see how the menu item is associated with this function using
        /// OleMenuCommandService service and MenuCommand class.
        /// </summary>
        /// <param name="sender">Event sender.</param>
        /// <param name="e">Event args.</param>
        private void Execute(object sender, EventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            string message = string.Format(CultureInfo.CurrentCulture, "Inside {0}.MenuItemCallback()", this.GetType().FullName);
            string title = "Command1";

            // Show a message box to prove we were here
            VsShellUtilities.ShowMessageBox(
                this.package,
                message,
                title,
                OLEMSGICON.OLEMSGICON_INFO,
                OLEMSGBUTTON.OLEMSGBUTTON_OK,
                OLEMSGDEFBUTTON.OLEMSGDEFBUTTON_FIRST);
        }
    }
*/
}
