import datetime

from invoke import task, run


@task
def histogram(ignore=''):
    result = run('git rev-list --all')
    date_to_adds = {}
    date_to_deletes = {}
    for sha in result.stdout.split('\n'):
        result = run('git diff-excluding {1} {0}~1 {0} --numstat'.format(sha, ignore), hide=True)
        added, deleted = get_total(result.stdout)
        iso8601 = run('git log {0} --pretty=format:%ai -1'.format(sha), hide=True).stdout.strip()
        commit_date = datetime.datetime.strptime(iso8601, "%Y-%m-%dT%H:%M:%S %z").date()
        date_to_adds[commit_date] = date_to_adds.get(commit_date) + added
        date_to_deletes[commit_date] = date_to_deletes.get(commit_date) + deleted
    print date_to_adds
    print date_to_deletes


def get_total(output):
    try:
        return sum(int(line.split()[0]) for line in output.split('\n')), sum(int(line.split()[1]) for line in output.split('\n'))
    except:
        import ipdb; ipdb.set_trace()

