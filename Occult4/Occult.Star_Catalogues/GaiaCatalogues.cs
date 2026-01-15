using System.Windows.Forms;

namespace Occult.Star_Catalogues
{
	public class GaiaCatalogues
	{
		private static CreateGaiaCatalog CreateGaiaCatalogue;

		public static void Show_CreateGaiaCatalogue()
		{
			try
			{
				((Control)CreateGaiaCatalogue).Show();
			}
			catch
			{
				CreateGaiaCatalogue = new CreateGaiaCatalog();
				((Control)CreateGaiaCatalogue).Show();
			}
		}
	}
}
