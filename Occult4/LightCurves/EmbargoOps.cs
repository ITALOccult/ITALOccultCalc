using System.Collections.Generic;
using System.IO;
using System.Linq;
using Occult;

namespace LightCurves
{
	internal class EmbargoOps
	{
		internal static List<Embargoed> EmbargoedList = new List<Embargoed>();

		internal static string LightCurveReportsDirectory = Utilities.AppPath + "\\LightCurveReports";

		internal static string EmbargoFile = Utilities.AppPath + "\\LightCurveReports\\Embargoes.csv";

		internal static void ReadEmbargoes()
		{
			EmbargoedList.Clear();
			if (!File.Exists(EmbargoFile))
			{
				return;
			}
			using StreamReader streamReader = new StreamReader(EmbargoFile);
			do
			{
				string text = streamReader.ReadLine()!.Trim();
				if (!(text == Embargoed.Header_File) && text.Length != 0)
				{
					string[] array = text.Split(new char[1] { ',' });
					if (array.Length >= 7)
					{
						Embargoed embargoed = new Embargoed();
						embargoed.AsteroidNumber = array[0];
						int.TryParse(array[1], out var result);
						embargoed.YearStart = result;
						int.TryParse(array[2], out result);
						embargoed.MonthStart = result;
						int.TryParse(array[3], out result);
						embargoed.YearEnd = result;
						int.TryParse(array[4], out result);
						embargoed.MonthEnd = result;
						embargoed.Requestor = array[5];
						embargoed.ContactInfo = array[6];
						EmbargoedList.Add(embargoed);
					}
				}
			}
			while (!streamReader.EndOfStream);
		}

		internal static void SaveListOfEmbargoes()
		{
			using StreamWriter streamWriter = new StreamWriter(EmbargoFile);
			streamWriter.WriteLine(Embargoed.Header_File);
			for (int i = 0; i < EmbargoedList.Count(); i++)
			{
				streamWriter.WriteLine(EmbargoedList[i].ToString());
			}
		}

		internal static void SetClearEmbargoStatus()
		{
			string[] files = Directory.GetFiles(LightCurveReportsDirectory, "*.embargoed");
			for (int i = 0; i < files.Length; i++)
			{
				SetEmbargoStatus(files[i]);
			}
			files = Directory.GetFiles(LightCurveReportsDirectory, "*.dat");
			for (int i = 0; i < files.Length; i++)
			{
				SetEmbargoStatus(files[i]);
			}
			files = Directory.GetFiles(LightCurveReportsDirectory, "*.init");
			for (int i = 0; i < files.Length; i++)
			{
				SetEmbargoStatus(files[i]);
			}
			files = Directory.GetFiles(LightCurveReportsDirectory, "*.del");
			for (int i = 0; i < files.Length; i++)
			{
				SetEmbargoStatus(files[i]);
			}
		}

		private static void SetEmbargoStatus(string F)
		{
			string fileName = Path.GetFileName(F);
			fileName.Contains("65803");
			bool flag = false;
			int num = fileName.IndexOf("(");
			if (num < 0)
			{
				return;
			}
			int num2 = fileName.IndexOf(")_", num);
			if (num2 <= num)
			{
				return;
			}
			string text = fileName.Substring(num + 1, num2 - num - 1);
			int.TryParse(fileName.Substring(num2 + 2, 4), out var result);
			int.TryParse(fileName.Substring(num2 + 6, 2), out var result2);
			double num3 = Utilities.JD_from_Date(result, result2, 15.0);
			flag = false;
			for (int i = 0; i < EmbargoedList.Count; i++)
			{
				if (text == EmbargoedList[i].AsteroidNumber && num3 > EmbargoedList[i].JDstartEmbargo && num3 < EmbargoedList[i].JDendEmbargo)
				{
					flag = true;
				}
			}
			if (flag)
			{
				if (fileName.EndsWith(".dat") | fileName.EndsWith(".init") | fileName.EndsWith(".dat"))
				{
					File.Move(F, LightCurveReportsDirectory + "\\" + fileName + ".embargoed");
				}
			}
			else if (fileName.EndsWith(".embargoed"))
			{
				File.Move(F, LightCurveReportsDirectory + "\\" + fileName.Replace(".embargoed", ""));
			}
		}
	}
}
