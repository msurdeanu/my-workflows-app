--- The pages of the parameters and of the placeholders are kept to admins (@RolesAllowed), so a user who is no admin
--- was offered two entries which could only end in an error.
UPDATE menu_items SET role = 'ROLE_ADMIN'
WHERE label IN ('menu.main.workflow-params', 'menu.main.workflow-placeholders');
