using System.Collections.Generic;
using System.IO;

namespace Occult.Asteroids
{
	internal class AsteroidClassList
	{
		public static string SourceFile = Utilities.AppPath + "\\Resource Files\\AsteroidClasses.csv";

		internal static List<AsteroidClass> AClass = new List<AsteroidClass>();

		public static void Fill_AllAsteroids()
		{
			AClass.Clear();
			if (!File.Exists(SourceFile))
			{
				return;
			}
			using StreamReader streamReader = new StreamReader(SourceFile);
			while (!streamReader.EndOfStream)
			{
				AsteroidClass asteroidClass = new AsteroidClass();
				asteroidClass.ReadCSVline(streamReader.ReadLine());
				AClass.Add(asteroidClass);
			}
		}

		internal static string ClassOfAnAsteroid(int AsteroidNumber)
		{
			if (AClass.Count < 1)
			{
				Fill_AllAsteroids();
			}
			if (AClass.Count < 1)
			{
				return "";
			}
			int num = 0;
			int num2 = AClass.Count - 1;
			do
			{
				int num3 = (num2 + num) / 2;
				if (AClass[num3].Number == AsteroidNumber)
				{
					return AClass[num3].AstClass;
				}
				if (AClass[num3].Number > AsteroidNumber)
				{
					num2 = num3 - 1;
				}
				else
				{
					num = num3 + 1;
				}
			}
			while (num2 >= num);
			return "";
		}
	}
}
