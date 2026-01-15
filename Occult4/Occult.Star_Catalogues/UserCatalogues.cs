using System.Windows.Forms;

namespace Occult.Star_Catalogues
{
	public class UserCatalogues
	{
		private static CreateUserCatalogue CreateUser;

		public static void Show_CreateUser()
		{
			try
			{
				((Control)CreateUser).Show();
			}
			catch
			{
				CreateUser = new CreateUserCatalogue();
				((Control)CreateUser).Show();
			}
		}
	}
}
