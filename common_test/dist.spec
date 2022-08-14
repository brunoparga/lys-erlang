{node, a, 'a@shapeshifter.local'}.
{node, b, 'b@shapeshifter.local'}.

{alias, demo, "./demo/"}.
{alias, meeting, "./meeting/"}.

{logdir, all_nodes, "./logs/"}.
{logdir, master, "./logs/"}.

{suites, [b], meeting, all}.
{suites, [a], demo, all}.
{skip_cases, [a], demo, basic_SUITE, test2, "This test fails on purpose."}.~