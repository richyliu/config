__all__ = ["configure"]

def configure(repl):
    repl.vi_mode = True
    repl.confirm_exit = False
    repl.prompt_style = "ipython"
    repl.show_signature = True
    repl.complete_while_typing = False
