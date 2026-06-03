For hosting, everything was quite easy through AWS:

- I got my domain through [AWS Route53](https://aws.amazon.com/route53/)'s domain registrar (~$15 USD / year).
- My files are uploaded to, and are statically served using [AWS Amplify](https://aws.amazon.com/amplify/) (~$0.50 USD / month).

I use the following script to automate zipping and uploading my files to AWS.

```bash
#!/usr/bin/env bash
set -euo pipefail

export AWS_PROFILE=update-amplify

# Check AWS Profie
aws sts get-caller-identity | cat
Get Amplify App details
aws amplify list-apps | cat

APP_ID=...
BRANCH=master
ZIP_FILE="$(date +%F).zip"

echo "Building..."
cabal run -fforce-recomp blog

echo "Packaging..."
(cd html && zip -r "../$ZIP_FILE" .)

echo "Deploying..."
DEPLOYMENT=$(
  aws amplify create-deployment \
    --app-id "$APP_ID" \
    --branch-name "$BRANCH" \
    --query '{jobId: jobId, uploadUrl: zipUploadUrl}' \
    --output json
)
JOB_ID=$(echo $DEPLOYMENT | jq -r '.jobId')
UPLOAD_URL=$(echo $DEPLOYMENT | jq -r '.uploadUrl')

curl -T $ZIP_FILE "$UPLOAD_URL"

aws amplify start-deployment --app-id $APP_ID --branch-name $BRANCH --job-id "$JOB_ID" | cat

mkdir -p published-versions
mv "$ZIP_FILE" published-versions
echo "Done; package in published-versions/$ZIP_FILE"
```

You just need to:
1. Install the [AWS CLI](https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html).
2. Create an [IAM](https://aws.amazon.com/iam/) user with the `AdministratorAccess-Amplify` policy (to get keys to use the CLI).
3. Create a matching profile i.e., files like:

**~/.aws/config**
```bash
[profile update-amplify]
region = ap-southeast-2
output = json
```
**~/.aws/credentials**
```bash
[update-amplify]
aws_access_key_id = ...
aws_secret_access_key = ...
```