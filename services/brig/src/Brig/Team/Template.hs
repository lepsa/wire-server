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

module Brig.Team.Template
  ( TeamTemplates (..),
    InvitationEmailTemplate (..),
    CreatorWelcomeEmailTemplate (..),
    MemberWelcomeEmailTemplate (..),
    loadTeamTemplates,

    -- * Re-exports
    Template,
    renderText,
    renderHtml,
  )
where

import Brig.Options
import Brig.Template
import Imports
import Wire.API.User.Identity

data InvitationEmailTemplate = InvitationEmailTemplate
  { url :: !Template,
    subject :: !Template,
    bodyText :: !Template,
    bodyHtml :: !Template,
    sender :: !Email,
    senderName :: !Text
  }

data CreatorWelcomeEmailTemplate = CreatorWelcomeEmailTemplate
  { url :: !Text,
    subject :: !Template,
    bodyText :: !Template,
    bodyHtml :: !Template,
    sender :: !Email,
    senderName :: !Text
  }

data MemberWelcomeEmailTemplate = MemberWelcomeEmailTemplate
  { url :: !Text,
    subject :: !Template,
    bodyText :: !Template,
    bodyHtml :: !Template,
    sender :: !Email,
    senderName :: !Text
  }

data TeamTemplates = TeamTemplates
  { invitationEmail :: !InvitationEmailTemplate,
    creatorWelcomeEmail :: !CreatorWelcomeEmailTemplate,
    memberWelcomeEmail :: !MemberWelcomeEmailTemplate
  }

loadTeamTemplates :: Opts -> IO (Localised TeamTemplates)
loadTeamTemplates o = readLocalesDir defLocale (templateDir gOptions) "team" $ \fp ->
  TeamTemplates
    <$> ( InvitationEmailTemplate tUrl
            <$> readTemplate fp "email/invitation-subject.txt"
            <*> readTemplate fp "email/invitation.txt"
            <*> readTemplate fp "email/invitation.html"
            <*> pure (emailSender gOptions)
            <*> readText fp "email/sender.txt"
        )
    <*> ( CreatorWelcomeEmailTemplate (creatorWelcomeUrl tOptions)
            <$> readTemplate fp "email/new-creator-welcome-subject.txt"
            <*> readTemplate fp "email/new-creator-welcome.txt"
            <*> readTemplate fp "email/new-creator-welcome.html"
            <*> pure (emailSender gOptions)
            <*> readText fp "email/sender.txt"
        )
    <*> ( MemberWelcomeEmailTemplate (memberWelcomeUrl tOptions)
            <$> readTemplate fp "email/new-member-welcome-subject.txt"
            <*> readTemplate fp "email/new-member-welcome.txt"
            <*> readTemplate fp "email/new-member-welcome.html"
            <*> pure (emailSender gOptions)
            <*> readText fp "email/sender.txt"
        )
  where
    gOptions = general (emailSMS o)
    tOptions = team (emailSMS o)
    tUrl = template $ invitationUrl tOptions
    defLocale = setDefaultTemplateLocale (optSettings o)
    readTemplate = readTemplateWithDefault (templateDir gOptions) defLocale "team"
    readText = readTextWithDefault (templateDir gOptions) defLocale "team"
