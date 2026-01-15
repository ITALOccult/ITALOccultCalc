using System.IO;
using System.Windows.Forms;

namespace Occult
{
	public class Elements
	{
		public static AsteroidElements_All MainAsteroids = new AsteroidElements_All(Utilities.AppPath + "\\Resource Files\\AsteroidElements.csv");

		public static AsteroidElements_All UserAsteroids = new AsteroidElements_All(Utilities.AppPath + "\\Resource Files\\UserMinorPlanetElements.csv");

		public static AsteroidElements_All UserAsteroids_Lunar = new AsteroidElements_All(Utilities.AppPath + "\\Resource Files\\UserMinorPlanetElements_Lunar.csv");

		public static AsteroidElements_All Reductions = new AsteroidElements_All(Utilities.AppPath + "\\Resource Files\\HorizonForReductions.csv");

		internal static void UpdateLunarUserFile()
		{
			//IL_005b: Unknown result type (might be due to invalid IL or missing references)
			string text = "";
			int result = 0;
			int num = -1;
			string text2 = Utilities.AppPath + "\\Resource Files\\UserMinorPlanetElements_Lunar.old";
			string text3 = Utilities.AppPath + "\\Resource Files\\UserMinorPlanetElements_Lunar.csv";
			if (File.Exists(text3) && new FileInfo(text3).Length < 90)
			{
				File.Delete(text3);
			}
			if (!File.Exists(text3))
			{
				((Form)new UserAsteroids_Edit(LunarOccultations: true)).ShowDialog();
				return;
			}
			if (MainAsteroids.AstElements.Count < 1)
			{
				MainAsteroids.Fill_AllAsteroids();
			}
			if (File.Exists(text2))
			{
				File.Delete(text2);
			}
			if (File.Exists(text3))
			{
				File.Move(text3, text2);
			}
			using StreamWriter streamWriter = new StreamWriter(text3);
			using StreamReader streamReader = new StreamReader(text2);
			text = streamReader.ReadLine();
			streamWriter.WriteLine(AsteroidElements.CSVHeader());
			while (!streamReader.EndOfStream)
			{
				text = streamReader.ReadLine();
				int.TryParse(text.Split(new char[1] { ',' })[0], out result);
				if (result == 0)
				{
					streamWriter.WriteLine(text);
				}
				else
				{
					num = MainAsteroids.GetAsteroidRecord_fromNumber(result);
					if (num >= 0)
					{
						streamWriter.WriteLine(MainAsteroids.AstElements[num].CSVString());
					}
					else
					{
						streamWriter.WriteLine(text);
					}
				}
				if (streamReader.EndOfStream)
				{
					break;
				}
			}
		}
	}
}
