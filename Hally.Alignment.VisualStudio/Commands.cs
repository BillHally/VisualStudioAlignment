﻿using System.ComponentModel.Composition;
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
using System;

namespace Hally.Alignment.VisualStudio
{
    public class AlignArgs                      : EditorCommandArgs { public AlignArgs                     (ITextView v, ITextBuffer b) : base(v, b) { } }
    public class AlignExtendedArgs              : EditorCommandArgs { public AlignExtendedArgs             (ITextView v, ITextBuffer b) : base(v, b) { } }
    public class RealignArgs                    : EditorCommandArgs { public RealignArgs                   (ITextView v, ITextBuffer b) : base(v, b) { } }
    public class RealignExtendedArgs            : EditorCommandArgs { public RealignExtendedArgs           (ITextView v, ITextBuffer b) : base(v, b) { } }
    public class RealignToFirstLineArgs         : EditorCommandArgs { public RealignToFirstLineArgs        (ITextView v, ITextBuffer b) : base(v, b) { } }
    public class RealignToFirstLineExtendedArgs : EditorCommandArgs { public RealignToFirstLineExtendedArgs(ITextView v, ITextBuffer b) : base(v, b) { } }
    public class UnalignArgs                    : EditorCommandArgs { public UnalignArgs                   (ITextView v, ITextBuffer b) : base(v, b) { } }

    public class CommandBindings
    {
        private const string CommandSetValue = "7404ba69-b772-4931-8783-7d904aefa62c";

        private const int AlignId                      = 0x2110;
        private const int AlignExtendedId              = 0x2120;
        private const int RealignId                    = 0x2130;
        private const int RealignExtendedId            = 0x2140;
        private const int RealignToFirstLineId         = 0x2150;
        private const int RealignToFirstLineExtendedId = 0x2160;
        private const int UnalignId                    = 0x2170;

        [Export]
        [CommandBinding(CommandSetValue, AlignId, typeof(AlignArgs))]
        internal CommandBindingDefinition alignBinding;

        [Export]
        [CommandBinding(CommandSetValue, AlignExtendedId, typeof(AlignExtendedArgs))]
        internal CommandBindingDefinition alignExtendedBinding;

        [Export]
        [CommandBinding(CommandSetValue, RealignId, typeof(RealignArgs))]
        internal CommandBindingDefinition realignBinding;

        [Export]
        [CommandBinding(CommandSetValue, RealignExtendedId, typeof(RealignExtendedArgs))]
        internal CommandBindingDefinition realignExtendedBinding;

        [Export]
        [CommandBinding(CommandSetValue, RealignToFirstLineId, typeof(RealignToFirstLineArgs))]
        internal CommandBindingDefinition realignToFirstLineBinding;

        [Export]
        [CommandBinding(CommandSetValue, RealignToFirstLineExtendedId, typeof(RealignToFirstLineExtendedArgs))]
        internal CommandBindingDefinition raalignToFirstLineExtendedBinding;

        [Export]
        [CommandBinding(CommandSetValue, UnalignId, typeof(UnalignArgs))]
        internal CommandBindingDefinition unalignBinding;
    }

    public abstract class AlignmentCommandHandler<TEditorCommandArgs> : ICommandHandler<TEditorCommandArgs> where TEditorCommandArgs : EditorCommandArgs
    {
        protected AlignmentCommandHandler(string displayName, Func<string[], string[]> align)
        {
            DisplayName = displayName;
            this.align = align;
        }

        [Import]
        private IEditorOperationsFactoryService EditorOperations = null;
        private readonly Func<string[], string[]> align;

        public string DisplayName { get; }

        public CommandState GetCommandState(TEditorCommandArgs args)
        {
            return args.TextView.Selection.IsEmpty ? CommandState.Unavailable : CommandState.Available;
        }

        public bool ExecuteCommand(TEditorCommandArgs args, CommandExecutionContext context)
        {
            using (context.OperationContext.AddScope(allowCancellation: false, description: "Aligning selected lines"))
            {
                AlignLines.ProcessSelectedLines(args.TextView, EditorOperations.GetEditorOperations(args.TextView), align);
            }

            return true;
        }
    }

    [Export(typeof(ICommandHandler))]
    [ContentType("text")]
    [Name(nameof(AlignCommandHandler))]
    public class AlignCommandHandler : AlignmentCommandHandler<AlignArgs>
    {
        AlignCommandHandler()
            : base("Align Selected Lines", Alignment.AlignAll)
        {
        }
    }

    [Export(typeof(ICommandHandler))]
    [ContentType("text")]
    [Name(nameof(AlignExtendedCommandHandler))]
    public class AlignExtendedCommandHandler : AlignmentCommandHandler<AlignExtendedArgs>
    {
        public AlignExtendedCommandHandler()
            : base("Align Selected Lines (Including Extended Tokens)", Alignment.AlignAllExtended)
        {
        }
    }

    [Export(typeof(ICommandHandler))]
    [ContentType("text")]
    [Name(nameof(RealignCommandHandler))]
    public class RealignCommandHandler : AlignmentCommandHandler<RealignArgs>
    {
        public RealignCommandHandler()
            : base("Realign Selected Lines", Alignment.RealignAll)
        {
        }
    }

    [Export(typeof(ICommandHandler))]
    [ContentType("text")]
    [Name(nameof(RealignExtendedCommandHandler))]
    public class RealignExtendedCommandHandler : AlignmentCommandHandler<RealignExtendedArgs>
    {
        public RealignExtendedCommandHandler()
            : base("Realign Selected Lines (Extended)", Alignment.RealignAllExtended)
        {
        }
    }

    [Export(typeof(ICommandHandler))]
    [ContentType("text")]
    [Name(nameof(RealignToFirstLineCommandHandler))]
    public class RealignToFirstLineCommandHandler : AlignmentCommandHandler<RealignToFirstLineArgs>
    {
        public RealignToFirstLineCommandHandler()
            : base("Realign Selected Lines to First Line", Alignment.RealignToFirstLine)
        {
        }
    }

    [Export(typeof(ICommandHandler))]
    [ContentType("text")]
    [Name(nameof(RealignToFirstLineExtendedCommandHandler))]
    public class RealignToFirstLineExtendedCommandHandler : AlignmentCommandHandler<RealignToFirstLineExtendedArgs>
    {
        public RealignToFirstLineExtendedCommandHandler()
            : base("Realign Selected Lines to First Line (Including Extended Tokens)", Alignment.RealignToFirstLineExtended)
        {
        }
    }

    [Export(typeof(ICommandHandler))]
    [ContentType("text")]
    [Name(nameof(UnalignCommandHandler))]
    public class UnalignCommandHandler : AlignmentCommandHandler<UnalignArgs>
    {
        public UnalignCommandHandler()
            : base("Unalign Selected Lines", Alignment.UnalignAll)
        {
        }
    }
}
