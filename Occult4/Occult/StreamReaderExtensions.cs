using System.IO;
using System.Reflection;

namespace Occult
{
	public static class StreamReaderExtensions
	{
		private static readonly FieldInfo charPosField = typeof(StreamReader).GetField("charPos", BindingFlags.DeclaredOnly | BindingFlags.Instance | BindingFlags.NonPublic);

		private static readonly FieldInfo byteLenField = typeof(StreamReader).GetField("byteLen", BindingFlags.DeclaredOnly | BindingFlags.Instance | BindingFlags.NonPublic);

		private static readonly FieldInfo charBufferField = typeof(StreamReader).GetField("charBuffer", BindingFlags.DeclaredOnly | BindingFlags.Instance | BindingFlags.NonPublic);

		public static long GetPosition(StreamReader reader)
		{
			int num = (int)byteLenField.GetValue(reader);
			long num2 = reader.BaseStream.Position - num;
			int num3 = (int)charPosField.GetValue(reader);
			if (num3 > 0)
			{
				char[] chars = (char[])charBufferField.GetValue(reader);
				int num4 = reader.CurrentEncoding.GetBytes(chars, 0, num3).Length;
				num2 += num4;
			}
			return num2;
		}

		public static void SetPosition(StreamReader reader, long position)
		{
			reader.DiscardBufferedData();
			reader.BaseStream.Seek(position, SeekOrigin.Begin);
		}
	}
}
