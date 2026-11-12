# Railbird constellation

## Scope
- Use this guide for requests involving railbird backend/main repo and railbird mobile app work.

## Related packages/projects (trigger list)
- If any of these names are mentioned, open this guide for context.
- `railbird`: primary backend/main railbird repository.
- `railbird-mobile`: primary mobile app repository.
- `railbird2`: alternate/new-generation backend repo.
- `railbird-mobile2`: alternate/new-generation mobile repo.
- `railbird-docs`: documentation repository.
- `railbird-landing-page`: marketing/landing site repository.
- `railbird-alert-tuning`: alert/tuning and operational experimentation repo.
- `railbird-agents-architecture`: architecture notes/prototypes for agent workflows.

## Symlink targets
- `./project-links/railbird` -> primary railbird repo.
- `./project-links/railbird-mobile` -> railbird mobile app repo.

## Discovery hints
- Start from `~/projects`.
- Common backend location is `~/projects/railbird`.
- Mobile repo often also lives under `~/projects`, but name/path may vary by machine.

## Read-first docs
- `./project-links/railbird/README.md`
- `./project-links/railbird-mobile/README.md` (if present)

## GKE access
- Use the `ivan@railbird.ai` gcloud account for railbird GKE. The active
  gcloud account (`ivanmalison@gmail.com`) has no RBAC in `railbird-dev-api`,
  so `just kube ...` fails with Forbidden.
- Don't switch gcloud's global account (other sessions share it). Pass a token:
  `kubectl --kubeconfig "$RB_SECRET_KUBECONFIG_YAML" --context railbird-gke -n railbird-dev-api --token "$(gcloud auth print-access-token --account=ivan@railbird.ai)" ...`
  inside `direnv exec` in the railbird repo.
