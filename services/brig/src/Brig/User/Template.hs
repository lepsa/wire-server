-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Brig.User.Template
  ( UserTemplates (..),
    ActivationSmsTemplate (..),
    VerificationEmailTemplate (..),
    ActivationEmailTemplate (..),
    TeamActivationEmailTemplate (..),
    ActivationCallTemplate (..),
    PasswordResetSmsTemplate (..),
    PasswordResetEmailTemplate (..),
    LoginSmsTemplate (..),
    LoginCallTemplate (..),
    DeletionSmsTemplate (..),
    DeletionEmailTemplate (..),
    NewClientEmailTemplate (..),
    SecondFactorVerificationEmailTemplate (..),
    loadUserTemplates,

    -- * Re-exports
    Template,
    renderText,
    renderHtml,
  )
where

import Brig.Options qualified as Opt
import Brig.Template
import Imports
import Wire.API.User.Identity

data UserTemplates = UserTemplates
  { activationSms :: !ActivationSmsTemplate,
    activationCall :: !ActivationCallTemplate,
    verificationEmail :: !VerificationEmailTemplate,
    activationEmail :: !ActivationEmailTemplate,
    activationEmailUpdate :: !ActivationEmailTemplate,
    teamActivationEmail :: !TeamActivationEmailTemplate,
    passwordResetSms :: !PasswordResetSmsTemplate,
    passwordResetEmail :: !PasswordResetEmailTemplate,
    loginSms :: !LoginSmsTemplate,
    loginCall :: !LoginCallTemplate,
    deletionSms :: !DeletionSmsTemplate,
    deletionEmail :: !DeletionEmailTemplate,
    newClientEmail :: !NewClientEmailTemplate,
    verificationLoginEmail :: !SecondFactorVerificationEmailTemplate,
    verificationScimTokenEmail :: !SecondFactorVerificationEmailTemplate,
    verificationTeamDeletionEmail :: !SecondFactorVerificationEmailTemplate
  }

data ActivationSmsTemplate = ActivationSmsTemplate
  { lUrl :: !Template,
    text :: !Template,
    sender :: !Text
  }

data ActivationCallTemplate = ActivationCallTemplate
  { text :: !Template
  }

data VerificationEmailTemplate = VerificationEmailTemplate
  { url :: !Template,
    subject :: !Template,
    bodyText :: !Template,
    bodyHtml :: !Template,
    sender :: !Email,
    senderName :: !Text
  }

data ActivationEmailTemplate = ActivationEmailTemplate
  { url :: !Template,
    subject :: !Template,
    bodyText :: !Template,
    bodyHtml :: !Template,
    sender :: !Email,
    senderName :: !Text
  }

data TeamActivationEmailTemplate = TeamActivationEmailTemplate
  { url :: !Template,
    subject :: !Template,
    bodyText :: !Template,
    bodyHtml :: !Template,
    sender :: !Email,
    senderName :: !Text
  }

data DeletionEmailTemplate = DeletionEmailTemplate
  { url :: !Template,
    subject :: !Template,
    bodyText :: !Template,
    bodyHtml :: !Template,
    sender :: !Email,
    senderName :: !Text
  }

data PasswordResetEmailTemplate = PasswordResetEmailTemplate
  { url :: !Template,
    subject :: !Template,
    bodyText :: !Template,
    bodyHtml :: !Template,
    sender :: !Email,
    senderName :: !Text
  }

data PasswordResetSmsTemplate = PasswordResetSmsTemplate
  { text :: !Template,
    sender :: !Text
  }

data LoginSmsTemplate = LoginSmsTemplate
  { url :: !Template,
    text :: !Template,
    sender :: !Text
  }

data LoginCallTemplate = LoginCallTemplate
  { text :: !Template
  }

data DeletionSmsTemplate = DeletionSmsTemplate
  { url :: !Template,
    text :: !Template,
    sender :: !Text
  }

data NewClientEmailTemplate = NewClientEmailTemplate
  { subject :: !Template,
    bodyText :: !Template,
    bodyHtml :: !Template,
    sender :: !Email,
    senderName :: !Text
  }

data SecondFactorVerificationEmailTemplate = SecondFactorVerificationEmailTemplate
  { subject :: !Template,
    bodyText :: !Template,
    bodyHtml :: !Template,
    sender :: !Email,
    senderName :: !Text
  }

loadUserTemplates :: Opt.Opts -> IO (Localised UserTemplates)
loadUserTemplates o = readLocalesDir defLocale templateDir "user" $ \fp ->
  UserTemplates
    <$> ( ActivationSmsTemplate smsActivationUrl
            <$> readTemplate fp "sms/activation.txt"
            <*> pure smsSender
        )
    <*> ( ActivationCallTemplate
            <$> readTemplate fp "call/activation.txt"
        )
    <*> ( VerificationEmailTemplate activationUrl
            <$> readTemplate fp "email/verification-subject.txt"
            <*> readTemplate fp "email/verification.txt"
            <*> readTemplate fp "email/verification.html"
            <*> pure emailSender
            <*> readText fp "email/sender.txt"
        )
    <*> ( ActivationEmailTemplate activationUrl
            <$> readTemplate fp "email/activation-subject.txt"
            <*> readTemplate fp "email/activation.txt"
            <*> readTemplate fp "email/activation.html"
            <*> pure emailSender
            <*> readText fp "email/sender.txt"
        )
    <*> ( ActivationEmailTemplate activationUrl
            <$> readTemplate fp "email/update-subject.txt"
            <*> readTemplate fp "email/update.txt"
            <*> readTemplate fp "email/update.html"
            <*> pure emailSender
            <*> readText fp "email/sender.txt"
        )
    <*> ( TeamActivationEmailTemplate teamActivationUrl
            <$> readTemplate fp "email/team-activation-subject.txt"
            <*> readTemplate fp "email/team-activation.txt"
            <*> readTemplate fp "email/team-activation.html"
            <*> pure emailSender
            <*> readText fp "email/sender.txt"
        )
    <*> ( PasswordResetSmsTemplate
            <$> readTemplate fp "sms/password-reset.txt"
            <*> pure smsSender
        )
    <*> ( PasswordResetEmailTemplate passwordResetUrl
            <$> readTemplate fp "email/password-reset-subject.txt"
            <*> readTemplate fp "email/password-reset.txt"
            <*> readTemplate fp "email/password-reset.html"
            <*> pure emailSender
            <*> readText fp "email/sender.txt"
        )
    <*> ( LoginSmsTemplate smsActivationUrl
            <$> readTemplate fp "sms/login.txt"
            <*> pure smsSender
        )
    <*> ( LoginCallTemplate
            <$> readTemplate fp "call/login.txt"
        )
    <*> ( DeletionSmsTemplate deletionUserUrl
            <$> readTemplate fp "sms/deletion.txt"
            <*> pure smsSender
        )
    <*> ( DeletionEmailTemplate deletionUserUrl
            <$> readTemplate fp "email/deletion-subject.txt"
            <*> readTemplate fp "email/deletion.txt"
            <*> readTemplate fp "email/deletion.html"
            <*> pure emailSender
            <*> readText fp "email/sender.txt"
        )
    <*> ( NewClientEmailTemplate
            <$> readTemplate fp "email/new-client-subject.txt"
            <*> readTemplate fp "email/new-client.txt"
            <*> readTemplate fp "email/new-client.html"
            <*> pure emailSender
            <*> readText fp "email/sender.txt"
        )
    <*> ( SecondFactorVerificationEmailTemplate
            <$> readTemplate fp "email/verification-login-subject.txt"
            <*> readTemplate fp "email/verification-login.txt"
            <*> readTemplate fp "email/verification-login.html"
            <*> pure emailSender
            <*> readText fp "email/sender.txt"
        )
    <*> ( SecondFactorVerificationEmailTemplate
            <$> readTemplate fp "email/verification-scim-token-subject.txt"
            <*> readTemplate fp "email/verification-scim-token.txt"
            <*> readTemplate fp "email/verification-scim-token.html"
            <*> pure emailSender
            <*> readText fp "email/sender.txt"
        )
    <*> ( SecondFactorVerificationEmailTemplate
            <$> readTemplate fp "email/verification-delete-team-subject.txt"
            <*> readTemplate fp "email/verification-delete-team.txt"
            <*> readTemplate fp "email/verification-delete-team.html"
            <*> pure emailSender
            <*> readText fp "email/sender.txt"
        )
  where
    gOptions = Opt.general $ Opt.emailSMS o
    uOptions = Opt.user $ Opt.emailSMS o
    tOptions = Opt.team $ Opt.emailSMS o
    emailSender = Opt.emailSender gOptions
    smsSender = Opt.smsSender gOptions
    smsActivationUrl = template $ Opt.smsActivationUrl uOptions
    activationUrl = template uOptions.activationUrl
    teamActivationUrl = template tOptions.activationUrl
    passwordResetUrl = template $ Opt.passwordResetUrl uOptions
    deletionUserUrl = template $ Opt.deletionUrl uOptions
    defLocale = Opt.setDefaultTemplateLocale (Opt.optSettings o)
    templateDir = Opt.templateDir gOptions
    readTemplate = readTemplateWithDefault templateDir defLocale "user"
    readText = readTextWithDefault templateDir defLocale "user"
