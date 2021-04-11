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
        public static void ProcessSelectedLines(ITextView textView, IEditorOperations editorOperations, Func<string, string> align)
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
                var updatedText = align.Invoke(text);
                var updatedSnapshot = textView.TextBuffer.Replace(selectedSpan, updatedText);

                // Redo the original selection, expanded or reduced to the extent of the updated text
                var updatedSnapshotSpan = new SnapshotSpan(updatedSnapshot, start, updatedText.Length);
                textView.Selection.Select(updatedSnapshotSpan, isReversed);

                // If the original selection had the caret at the start, move it back there now (the text replacement leaves the caret at the end,
                // and redoing the selection doesn't affect the caret)
                if (isReversed)
                {
                    textView.Caret.MoveTo(updatedSnapshotSpan.Start);
                }
#if DEBUG
            }
            catch (Exception ex) {
                textView.TextBuffer.Insert(selectedSpan.Span.Start, ex.ToString());
            }
#endif
        }
    }

    public class UnalignLinesCommandArgs : EditorCommandArgs
    {
        public UnalignLinesCommandArgs(ITextView textView, ITextBuffer textBuffer)
            : base(textView, textBuffer)
        {
        }
    }

    public class AlignLinesCommandArgs : EditorCommandArgs
    {
        public AlignLinesCommandArgs(ITextView textView, ITextBuffer textBuffer)
            : base(textView, textBuffer)
        {
        }
    }
    public class RealignLinesCommandArgs : EditorCommandArgs
    {
        public RealignLinesCommandArgs(ITextView textView, ITextBuffer textBuffer)
            : base(textView, textBuffer)
        {
        }
    }

    public class CommandBindings
    {
        private const string CommandSetValue = "7404ba69-b772-4931-8783-7d904aefa62c";

        private const int UnalignLinesCommandId = 0x2110;
        private const int AlignLinesCommandId   = 0x2120;
        private const int RealignLinesCommandId = 0x2130;

        [Export]
        [CommandBinding(CommandSetValue, UnalignLinesCommandId, typeof(UnalignLinesCommandArgs))]
        internal CommandBindingDefinition unalignLinesCommandBinding;

        [Export]
        [CommandBinding(CommandSetValue, AlignLinesCommandId, typeof(AlignLinesCommandArgs))]
        internal CommandBindingDefinition alignLinesCommandBinding;

        [Export]
        [CommandBinding(CommandSetValue, RealignLinesCommandId, typeof(RealignLinesCommandArgs))]
        internal CommandBindingDefinition realignLinesCommandBinding;
    }

    [Export(typeof(ICommandHandler))]
    [ContentType("text")]
    [Name(nameof(UnalignLinesCommandHandler))]
    public class UnalignLinesCommandHandler : ICommandHandler<UnalignLinesCommandArgs>
    {
        public string DisplayName => "Unalign Selected Lines";

        [Import]
        private IEditorOperationsFactoryService EditorOperations = null;

        public CommandState GetCommandState(UnalignLinesCommandArgs args)
        {
            return args.TextView.Selection.IsEmpty ? CommandState.Unavailable : CommandState.Available;
        }

        public bool ExecuteCommand(UnalignLinesCommandArgs args, CommandExecutionContext context)
        {
            using (context.OperationContext.AddScope(allowCancellation: false, description: "Realigning selected lines"))
            {
                AlignLines.ProcessSelectedLines(args.TextView, EditorOperations.GetEditorOperations(args.TextView), Alignment.UnalignAll);
            }

            return true;
        }
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
                AlignLines.ProcessSelectedLines(args.TextView, EditorOperations.GetEditorOperations(args.TextView), Alignment.AlignAll);
            }

            return true;
        }
    }

    [Export(typeof(ICommandHandler))]
    [ContentType("text")]
    [Name(nameof(RealignLinesCommandHandler))]
    public class RealignLinesCommandHandler : ICommandHandler<RealignLinesCommandArgs>
    {
        public string DisplayName => "Realign Selected Lines";

        [Import]
        private IEditorOperationsFactoryService EditorOperations = null;

        public CommandState GetCommandState(RealignLinesCommandArgs args)
        {
            return args.TextView.Selection.IsEmpty ? CommandState.Unavailable : CommandState.Available;
        }

        public bool ExecuteCommand(RealignLinesCommandArgs args, CommandExecutionContext context)
        {
            using (context.OperationContext.AddScope(allowCancellation: false, description: "Realigning selected lines"))
            {
                AlignLines.ProcessSelectedLines(args.TextView, EditorOperations.GetEditorOperations(args.TextView), Alignment.RealignAll);
            }

            return true;
        }
    }
}
