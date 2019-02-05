from fontawesome import icons as fa
import i3ipc
import logging

logger = logging.getLogger(__name__)


def get_icon(window_class):
    default_icon = fa['window-maximize']
    try:
        return {
            'bitwarden': fa['shield'],
            'chromium-browser': fa['chrome'],
            'cisco anyconnect secure mobility client': fa['lock'],
            'emacs': fa['code'],
            'firefox': fa['firefox'],
            'gnome-control-center': fa['cog'],
            'gnome-terminal': fa['terminal'],
            'nautilus': fa['folder-open'],
            'qutebrowser': fa['compass'],
            'spotify': fa['spotify'],
        }[window_class.lower()]
    except KeyError:
        logger.warn('Unknown class: ', window_class)
        return default_icon


class I3:
    def __init__(self):
        self.i3 = i3ipc.Connection()

    def command(self, cmd):
        logger.info(cmd)
        self.i3.command(cmd)

    def focused_workspace(self):
        return self.tree.find_focused().workspace()

    def move_workspace(self, direction, steps=1):
        offset = steps
        if direction == 'left':
            offset *= -1

        ws = self.focused_workspace()
        self.swap_workspaces(ws.num, ws.num + offset)

    def assign_workspace_names(self):
        for idx, ws in enumerate(self.workspaces(), 1):
            new_name = '%d' % idx

            icons = [get_icon(win.window_class) for win in ws.leaves()]
            if icons:
                new_name += ': %s' % '  '.join(icons)

            name = ws.name
            if name != new_name:
                self.rename_workspace(name, new_name)

    def has_workspace(self, num):
        return any(ws.num == num for ws in self.workspaces())

    def rename_workspace(self, src, dst):
        self.command('rename workspace "%s" to "%s"' % (src, dst))

    def renumber_workspace(self, src, dst):
        for ws in self.workspaces():
            if ws.num == src:
                new_name = ws.name.replace(str(src), str(dst))
                self.rename_workspace(ws.name, new_name)

    def swap_workspaces(self, a, b):
        if not self.has_workspace(a) or not self.has_workspace(b):
            return

        self.renumber_workspace(a, 0)
        self.renumber_workspace(b, a)
        self.renumber_workspace(0, b)

    def workspace_number(self, name):
        for ws in self.workspaces():
            if ws.name == name:
                return ws.num
        return None

    @property
    def tree(self):
        return self.i3.get_tree()

    def workspaces(self):
        return self.tree.workspaces()

    def run(self):
        def on_workspace(i3, e):
            if e.change in ('empty', 'init'):
                self.assign_workspace_names()

        def on_window(i3, e):
            if e.change in ('move', 'new', 'close'):
                self.assign_workspace_names()

        self.i3.on('workspace', on_workspace)
        self.i3.on('window', on_window)

        self.assign_workspace_names()

        try:
            self.i3.main()
        except KeyboardInterrupt:
            pass


def parse_args():
    import argparse

    parser = argparse.ArgumentParser(description='i3wm workspace manager')
    subparsers = parser.add_subparsers(metavar='Commands:')

    def error(*args):
        parser.error('Please specify a command.')

    def monitor(i3, args):
        i3.run()

    def move(i3, args):
        i3.move_workspace(args.direction)

    parser.set_defaults(func=error)

    parser_monitor = subparsers.add_parser('monitor', help='Monitor i3 for workspace/window changes.')
    parser_monitor.set_defaults(func=monitor)

    parser_move = subparsers.add_parser('move', help='Move workspace')
    parser_move.add_argument('direction', choices=('left', 'right'), help='Move left or right')
    parser_move.set_defaults(func=move)

    return parser.parse_args()


def main():
    args = parse_args()
    args.func(I3(), args)


if __name__ == '__main__':
    main()
